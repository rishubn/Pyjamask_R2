--------------------------------------------------------------------------------
--! @file       CryptoCore.vhd
--! @brief      Top level file for Pyjamask Cipher
--! @author     Rishub Nagpal <rnagpal2@gmu.edu>
--! @copyright  Copyright (c) 2020 Cryptographic Engineering Research Group
--!             ECE Department, George Mason University Fairfax, VA, U.S.A.
--!             All rights Reserved.
--! @license    This project is released under the GNU Public License.
--!             The license and distribution terms for this file may be
--!             found in the file LICENSE in this distribution or at
--!             http://www.gnu.org/licenses/gpl-3.0.txt
--! @note       This is publicly available encryption source code that falls
--!             under the License Exception TSU (Technology and software-
--!             unrestricted)
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_misc.all;
use work.NIST_LWAPI_pkg.all;
use work.design_pkg.all;

entity CryptoCore is
    Port (
        clk             : in   STD_LOGIC;
        rst             : in   STD_LOGIC;
        --PreProcessor===============================================
        ----!key----------------------------------------------------
        key             : in   STD_LOGIC_VECTOR (CCSW     -1 downto 0);
        key_valid       : in   STD_LOGIC;
        key_ready       : out  STD_LOGIC;
        ----!Data----------------------------------------------------
        bdi             : in   STD_LOGIC_VECTOR (CCW     -1 downto 0);
        bdi_valid       : in   STD_LOGIC;
        bdi_ready       : out  STD_LOGIC;
        bdi_pad_loc     : in   STD_LOGIC_VECTOR (CCWdiv8 -1 downto 0);
        bdi_valid_bytes : in   STD_LOGIC_VECTOR (CCWdiv8 -1 downto 0);
        bdi_size        : in   STD_LOGIC_VECTOR (3       -1 downto 0);
        bdi_eot         : in   STD_LOGIC;
        bdi_eoi         : in   STD_LOGIC;
        bdi_type        : in   STD_LOGIC_VECTOR (4       -1 downto 0);
        decrypt_in      : in   STD_LOGIC;
        key_update      : in   STD_LOGIC;
        hash_in         : in   std_logic;
        --!Post Processor=========================================
        bdo             : out  STD_LOGIC_VECTOR (CCW      -1 downto 0);
        bdo_valid       : out  STD_LOGIC;
        bdo_ready       : in   STD_LOGIC;
        bdo_type        : out  STD_LOGIC_VECTOR (4       -1 downto 0);
        bdo_valid_bytes : out  STD_LOGIC_VECTOR (CCWdiv8 -1 downto 0);
        end_of_block    : out  STD_LOGIC;
        msg_auth_valid  : out  STD_LOGIC;
        msg_auth_ready  : in   STD_LOGIC;
        msg_auth        : out  STD_LOGIC
    );
end CryptoCore;

architecture behavioral of CryptoCore is

    -- Number of words the respective blocks contain.
    constant NPUB_WORDS_C       : integer   := get_words(NPUB_SIZE, CCW);
    constant BLOCK_WORDS_C      : integer   := get_words(DBLK_SIZE, CCW);
    -- Number of address bits required to address the above blocks when stored as ram.
    constant ADDR_BITS_96_C     : integer   := log2_ceil(NPUB_WORDS_C);
    constant ADDR_BITS_128_C    : integer   := log2_ceil(BLOCK_WORDS_C);


    -- State signals
    type state_t is (RESET,
                    IDLE,
                    -- KEY and key dependent variable generation
                    LD_RND_KEYS,
                    WAIT_KEY_READY,
                    ABSORB_KEY,
                    PROCESS_KEY,
                    LD_LI,
                    GEN_LI,
                    GEN_LDOLLAR,
                    LD_LSTAR,
                    GEN_LSTAR,
                    --NONCE and initial constants generation
                    ABSORB_NPUB,
                    LD_NONCE,
                    LD_CIN_NONCE,
                    GEN_KTOP,
                    LD_CIN0,
                    GEN_OFFSET0,
                    -- Hash/Tag generation
                    WAIT_INPUT,
                    LD_OFFSET_AUTH_NULL,
                    GEN_L_NTZ_AD,
                    LD_OI_HASH,
                    LD_AD,
                    ABSORB_AD,
                    PADD_AD,
                    PADD_AD_10_WORD,
                    LD_CIN_AD,
                    GEN_OI_HASH,
                    GEN_AUTH,
                    LD_EMPTY_TAG,
                    LD_TAG,
                    GEN_TAG,
                    OUTPUT_TAG,
                    -- PT/CT generation
                    LD_OFFSET_0,
                    GEN_L_NTZ_MSG,
                    GEN_OI_MSG,
                    LD_MSG,
                    LD_CIN_MSG,
                    ABSORB_MSG,
                    PADD_MSG,
                    PADD_MSG_10_WORD,
                    GEN_OI_CT,
                    GEN_CT,
                    OUTPUT_DATA,
                    -- Tag Verification
                    ABSORB_TAG,
                    VERIFY_TAG,
                    TAG_MATCH_FAILURE,
                    TAG_MATCH_SUCCESS);
    signal n_state_s, state_s           : state_t;

    -- Counter for blocks will be casted to block number block_num_s according to specification
    signal block_cnt_s, block_cnt_r     : integer range 0 to 2**16;
    signal block_num_s                  : std_logic_vector(DBLK_SIZE - 1 downto 0);
    signal block_num_word_s             : std_logic_vector(CCW - 1 downto 0);

    -- Word counter for address generation. Increases every time a word is transferred.
    signal word_cnt_s, word_cnt_r          : integer range 0 to BLOCK_WORDS_C - 1;



    signal c_init : std_logic;
    signal c_start : std_logic;
    signal c_decrypt : std_logic;
    signal c_done : std_logic;
    signal c_done_init : std_logic;
    signal c_ready : std_logic;
    signal c_din_valid : std_logic;
    signal c_dout_valid : std_logic;
    signal c_done_mm : std_logic;
    signal c_din : std_logic_vector(CCW - 1 downto 0);
    signal c_dout : std_logic_vector(CCW - 1 downto 0);
    -- input registers and control signals
    -- bdi_eot 
    signal en_bdi_eot : std_logic;
    signal bdi_eot_r : std_logic;
    signal clr_bdi_eot : std_logic;
    -- bdi_eoi
    signal en_bdi_eoi : std_logic;
    signal clr_bdi_eoi : std_logic;
    signal bdi_eoi_r : std_logic;
    -- bdi partial
    signal bdi_partial_r : std_logic;
    signal en_bdi_partial : std_logic;
    signal clr_bdi_partial : std_logic;
    signal bdi_partial_s : std_logic;
    -- decrypt
    signal decrypt_in_r : std_logic;
    signal en_decrypt_in : std_logic;
    signal clr_decrypt_in : std_logic;
   
    --bdi mux
    signal bdi_mux : std_logic_vector(CCW - 1 downto 0);
    signal sel_bdi : std_logic_vector(1 downto 0);
    -- bdi offset mux
    signal bdi_offset_mux : std_logic_vector(0 to CCW - 1);
    signal sel_bdi_offset : std_logic;
    -- registers for padding signals
    signal bdi_size_r : std_logic_vector(11 downto 0);
    signal en_bdi_size : std_logic;
    signal en_bdi_pad_loc : std_logic;
    signal en_bdi_valid_bytes : std_logic;
    signal sel_bdi_pad_loc : std_logic;
    signal sel_bdi_valid_bytes : std_logic;
    signal sel_bdi_size : std_logic;
    signal bdi_size_mux : std_logic_vector(2 downto 0);
    signal bdi_size_s : std_logic_vector(2 downto 0);
    signal bdi_valid_bytes_r : std_logic_vector(15 downto 0);
    signal bdi_pad_loc_r : std_logic_vector(15 downto 0);
    signal bdi_valid_bytes_mux : std_logic_vector(3 downto 0);
    signal bdi_pad_loc_mux : std_logic_vector(3 downto 0);
    signal bdi_pad_loc_s : std_logic_vector(3 downto 0);
    signal bdi_valid_bytes_s : std_logic_vector(3 downto 0);

    signal bdo_valid_s : std_logic;
    
    signal bdo_valid_bytes_s : std_logic_vector(CCWdiv8 - 1 downto 0);
    signal sel_decrypt : std_logic;

    signal mask : std_logic_vector(0 to CCW - 1);
    signal bdi_s : std_logic_vector(0 to CCW - 1);
    signal bdi_sr : std_logic_vector(0 to DBLK_SIZE - 1);
    signal en_bdi_sr : std_logic;
    signal rotate_bdi_sr : std_logic;

    signal bottom_r, bottom_s : integer range 0 to 2**6 - 1;
    
    type arrayDBLK is array (0 to 2) of std_logic_vector(0 to CCW -1);
    signal sel_tag_offset : std_logic_vector(1 downto 0);
    signal tag_offset_mux_l : arrayDBLK;
    signal tag_offset_mux_r : arrayDBLK;

    signal nonce_mux : std_logic_vector(0 to CCW - 1);
    signal sel_nonce : std_logic_vector(1 downto 0);

    signal sel_cin : std_logic_vector(1 downto 0);
    signal en_c_din : std_logic;
    signal c_din_sr : std_logic_vector(0 to DBLK_SIZE - 1);
    signal cin_mux : std_logic_vector(0 to CCW - 1);
    
    signal rot_stretchhash : std_logic;
    signal en_stretchhash : std_logic;
    -- offset signals
    signal o0_s : std_logic_vector(0 to CCW - 1);
    signal rotate_o0 : std_logic;
    --lstar, ldollar, li signals
    signal li_addr : std_logic_vector(4 - 1 downto 0);
    signal li_s : std_logic_vector(0 to CCW - 1);
    signal li_din : std_logic_vector(0 to DBLK_SIZE - 1);
    signal li_dout : std_logic_vector(0 to DBLK_SIZE - 1);
    signal we_li : std_logic;
    signal l_cnt_r, l_cnt_s : integer range 0 to 2**4 - 1;
    signal li_in_mux : std_logic_vector(0 to CCW - 1);
    signal dbl_s : std_logic_vector(0 to DBLK_SIZE - 1);
    signal dbl_msb_s : std_logic;
    signal en_li_in : std_logic;
    signal en_li_out : std_logic;
    signal rotate_li : std_logic;
    signal sel_li : std_logic;
    signal sel_addr : std_logic;
    signal li_in_r : std_logic_vector(0 to DBLK_SIZE - 1);
    signal li_out_r : std_logic_vector(0 to DBLK_SIZE - 1);
    signal sel_li_addr : std_logic;

    signal lstar_r : std_logic_vector(0 to DBLK_SIZE - 1);
    signal lstar_s : std_logic_vector(0 to CCW - 1);
    signal rotate_lstar : std_logic;
    signal en_lstar : std_logic;
    -- hash/AD signals
    signal tag_match : std_logic;
    -- sum signals
    signal en_sum : std_logic;
    signal rotate_sum : std_logic;
    signal sum_r : std_logic_vector(0 to DBLK_SIZE -1);
    signal sel_sum : std_logic_vector(1 downto 0);
    signal sum_s : std_logic_vector(0 to CCW - 1);
    signal sum_mux : std_logic_vector(0 to CCW - 1);
    -- din sr
    signal c_din_ld : std_logic;
    -- bdo 
    signal sel_bdo_offset : std_logic;
    signal c_dout_r : std_logic_vector(0 to DBLK_SIZE - 1);
    signal c_dout_s : std_logic_vector(0 to CCW -1);
    signal rotate_c_dout_sr : std_logic;
    signal bdo_offset_mux : std_logic_vector(0 to CCW - 1);
    signal en_c_dout_sr : std_logic;
    signal sel_bdo : std_logic;
    
    signal we_reg : std_logic;
    signal reg_din : std_logic_vector(0 to CCW - 1);
    signal reg_dout : std_logic_vector(0 to CCW - 1);
    signal reg_addr : std_logic_vector(3 downto 0);
    signal sel_reg_din : std_logic_vector(1 downto 0);
    signal sel_reg_xor : std_logic;
    signal reg_xor_mux : std_logic_Vector(0 to CCW - 1);
    signal reg_cnt_r, reg_cnt_s : integer range 0 to 1;
begin
    --instances
    i_pyjamask_block_cipher : entity work.PyjamaskEncDec
        port map(
            clk => clk,
            rst => rst,
    
            key   => key,
            init  => c_init,
            start => c_start,
            din   => cin_mux,
            decrypt => c_decrypt,
            done      => c_done,
            done_init => c_done_init,
            ready     => c_ready,
            dout_valid => c_dout_valid,
            din_valid => c_din_valid,
            key_valid => key_valid,
            key_ready => key_ready,
            dout => c_dout
        );


    FSM_state_register : process(clk)
    begin
        if rising_edge(clk) then
            if (rst = '1') then
                state_s <= RESET;
            else
                state_s <= n_state_s;
            end if;
        end if;
    end process FSM_state_register;

    FSM_control_path : process(all)
    begin
        n_state_s <= state_s;
        word_cnt_s <= word_cnt_r;
        bottom_s <= bottom_r;
        block_cnt_s <= block_cnt_r;
        l_cnt_s <= l_cnt_r;
        reg_cnt_s <= reg_cnt_r;
        c_init <= '0';
        bdi_ready <= '0';
        sel_cin <= "00";
        c_start <= '0';
        en_lstar <= '0';
        sel_sum <= "00";
        en_sum <= '0';
        en_c_dout_sr <= '0';
        en_bdi_sr <= '0';
        bdo_valid_s <= '0';
        bdo_valid_bytes_s <= "0000";
        bdo_type <= "0000";
        end_of_block <= '0';
        sel_bdo_offset <= '0';
        en_bdi_eot <= '0';
        en_bdi_eoi <= '0';
        clr_bdi_eoi <= '0';
        clr_bdi_eot <= '0';
        msg_auth_valid <= '0';
        msg_auth <= '0';
        en_decrypt_in <= '0';
        sel_bdi <= "00";
        clr_bdi_partial <= '0';
        en_bdi_partial <= '0';
        sel_bdi_offset <= '0';
        c_din_ld <= '0';
        sel_nonce <= "00";
        sel_tag_offset <= (others => '0');
        en_stretchhash <= '0';
        rot_stretchhash <= '0';
        c_din_valid <= '0';
        en_bdi_size <= '0';
        sel_bdi_size <= '0';
        sel_bdo <= '0';
        clr_decrypt_in <= '0';
        sel_decrypt <= '0';
        en_bdi_pad_loc <= '0';
        en_bdi_valid_bytes <= '0';
        sel_bdi_pad_loc <= '0';
        sel_bdi_valid_bytes <= '0';
        sel_li <= '0';
        sel_li_addr <= '0';
        en_li_in <= '0';
        en_li_out <= '0';
        we_li <= '0';
        rotate_li <= '0';
        rotate_o0 <= '0';
        rotate_bdi_sr <= '0';
        rotate_sum <= '0';
        rotate_lstar <= '0';
        rotate_c_dout_sr <= '0';
        sel_reg_din <= "00";
        we_reg <= '0';
        sel_reg_xor <= '0';
        case state_s is
            when RESET =>
                n_state_s <= IDLE;
            when IDLE =>
                word_cnt_s <= 0;
                block_cnt_s <= 0;
                l_cnt_s <= 0;
                reg_cnt_s <= 0;
                sel_sum <= "10";
                en_sum <= '1';
                clr_bdi_partial <= '1';
                clr_decrypt_in <= '1';
                reg_cnt_s <= AUTH_ADDR_C;
                if key_update = '1' then
                    n_state_s <= LD_RND_KEYS;
                    word_cnt_s <= 0;
                    c_init <= '1';
                elsif bdi_valid = '1' then
                    en_decrypt_in <= '1';
                    if bdi_type = HDR_NPUB then
                        n_state_s <= ABSORB_NPUB;
                    end if;
                end if;
            -- Load key and generate round keys
            when LD_RND_KEYS =>
                if c_done_init = '1' then
                    c_start <= '1';
                    n_state_s <= GEN_LSTAR;
                end if;
            when GEN_LSTAR =>
                sel_cin <= "10";
                c_din_valid <= '1';
                if c_dout_valid = '1' then
                    sel_li <= '1';
                    en_li_in <= '1';
                    en_lstar <= '1';
                elsif c_done = '1' then
                    n_state_s <= LD_LI;
                end if;
            when LD_LI =>
                sel_li_addr <= '1';
                if word_cnt_r = 1 then      
                    en_li_out <= '1';
                    word_cnt_s <= 0;
                    n_state_s <= GEN_LI;
                else
                    we_li <= '1';
                    word_cnt_s <= word_cnt_r + 1;
                end if;
            when GEN_LI =>
            sel_li_addr <= '1';
                if l_cnt_r = 10 then
                    l_cnt_s <= 0;
                    n_state_s <= IDLE;
                else
                    if word_cnt_r = BLOCK_WORDS_C - 1 then
                        rotate_li <= '1';
                        en_li_in <= '1';
                        word_cnt_s <= 0;
                        l_cnt_s <= l_cnt_r + 1;
                        n_state_s <= LD_LI;
                    else    
                        en_li_in <= '1';
                        rotate_li <= '1';
                        word_cnt_s <= word_cnt_r + 1;
                    end if;
                end if;
            -- load npub and generate nonce+initial constants 
            when ABSORB_NPUB =>
                bdi_ready <= '1';
                if word_cnt_r = NPUB_WORDS_C - 1 and bdi_valid = '1' then
                    n_state_s <= LD_NONCE;
                    en_bdi_eoi <= '1';
                    c_start <= '1';
                    word_cnt_s <= 0;
                    en_bdi_sr <= '1';
                elsif bdi_valid = '1' then
                    en_bdi_sr <= '1';
                    word_cnt_s <= word_cnt_r + 1;
                end if;
            when LD_NONCE =>
                sel_cin <= "01";
                c_din_valid <= '1';
                we_reg <= '1';
                if word_cnt_r = 0 then
                    sel_nonce <= "01";
                    rotate_bdi_sr <= '1';
                    word_cnt_s <= word_cnt_r + 1;
                elsif (word_cnt_r = 1 or word_cnt_r = 2) then
                    word_cnt_s <= word_cnt_r + 1;
                    rotate_bdi_sr <= '1';
                elsif word_cnt_r = 3 then
                    sel_cin <= "01";
                    sel_nonce <= "10";
                    rotate_bdi_sr <= '1';
                    bottom_s <= to_integer(unsigned(bdi_s(CCW - 6 to CCW - 1)));
                    word_cnt_s <= 0;
                    c_start <= '1';
                    n_state_s <= GEN_KTOP;
                end if;
            -- Load nonce and initial constants
            when GEN_KTOP =>
                if c_dout_valid = '1' then
                    en_stretchhash <= '1';
                elsif c_done = '1' then
                    en_stretchhash <= '0';
                    n_state_s <= GEN_OFFSET0;
                end if;
            when GEN_OFFSET0 =>
              --  en_oi <= '1';
              --  sel_oi <= "01";
               -- rotate_o0  <= '1';
                if bottom_r = 0 then
                    rot_stretchhash <= '0';
                    if bdi_eoi_r = '1' then
                        reg_cnt_s <= OFFSET_ADDR_C;
                        n_state_s <= LD_OFFSET_0;
                    else
                        n_state_s <= WAIT_INPUT;
                    end if;
                else
                    rot_stretchhash <= '1';
                    bottom_s <= bottom_r - 1;
                end if;
            when WAIT_INPUT =>
                block_cnt_s <= 0;
                word_cnt_s <= 0;
                if bdi_type = HDR_AD then
                    n_state_s <= LD_OFFSET_AUTH_NULL;
                    reg_cnt_s <= AUTH_ADDR_C;
                elsif bdi_type = HDR_PT or bdi_type = HDR_CT then
                    reg_cnt_s <= OFFSET_ADDR_C;
                    n_state_s <= LD_OFFSET_0;
                end if;
            -- Load AD and generate hash
            when LD_OFFSET_AUTH_NULL =>
                sel_bdi_size <= '1';
                en_bdi_size <= '1'; -- zero out size register
                we_reg <= '1';
                if word_cnt_r = BLOCK_WORDS_C - 1 and reg_cnt_r = OFFSET_ADDR_C then
                    n_state_s <= ABSORB_AD;
                    word_cnt_s <= 0;
                elsif word_cnt_r = BLOCK_WORDS_C - 1 then
                    reg_cnt_s <= OFFSET_ADDR_C;
                    word_cnt_s <= 0;
                else
                    word_cnt_s <= word_cnt_r + 1;
                end if;
            when ABSORB_AD =>
                bdi_ready <= '1';
                reg_cnt_s <= OFFSET_ADDR_C;
                if bdi_partial_s = '1' and bdi_valid = '1' and word_cnt_r < BLOCK_WORDS_C - 1 then
                    word_cnt_s <= word_cnt_r + 1;
                    en_bdi_partial <= '1';
                    sel_bdi <= "01";
                    en_bdi_eoi <= '1';
                    en_bdi_sr <= '1';
                    en_bdi_eot <= '1';
                    n_state_s <= PADD_AD;
                elsif bdi_partial_s = '1' and bdi_valid = '1' and word_cnt_r = BLOCK_WORDS_C - 1 then
                    block_cnt_s <= block_cnt_r + 1;
                    word_cnt_s <= 0;
                    en_bdi_eoi <= '1';
                    en_bdi_sr <= '1';
                    en_bdi_eot <= '1';
                    sel_bdi <= "01";
                    en_bdi_sr <= '1';
                    en_bdi_partial <= '1';
                    c_start <= '1';
                    n_state_s <= LD_CIN_AD;
                elsif bdi_eot = '1' and bdi_valid = '1' and word_cnt_r < BLOCK_WORDS_C - 1 then
                    word_cnt_s <= word_cnt_r + 1;
                    en_bdi_partial <= '1';
                    en_bdi_eoi <= '1';
                    en_bdi_sr <= '1';
                    en_bdi_eot <= '1';
                    n_state_s <= PADD_AD_10_WORD;
                elsif word_cnt_r = BLOCK_WORDS_C - 1 and bdi_valid = '1' then
                    block_cnt_s <= block_cnt_r + 1;
                    word_cnt_s <= 0;
                    en_bdi_eoi <= '1';
                    en_bdi_sr <= '1';
                    en_bdi_eot <= '1';
                    n_state_s <= GEN_L_NTZ_AD;
                elsif bdi_valid = '1' then
                    en_bdi_sr <= '1';
                    word_cnt_s <= word_cnt_r + 1;
                end if;
            when PADD_AD_10_WORD =>
                sel_bdi <= "01";
                en_bdi_sr <= '1';
                if word_cnt_r = BLOCK_WORDS_C - 1 then
                    block_cnt_s <= block_cnt_r + 1;
                    word_cnt_s <= 0;
                    c_start <= '1';
                    n_state_s <= LD_CIN_AD;
                else
                    word_cnt_s <= word_cnt_r + 1;
                    n_state_s <= PADD_AD;
                end if;
            when PADD_AD =>
                sel_bdi <= "10";
                en_bdi_sr <= '1';
                if word_cnt_r = BLOCK_WORDS_C - 1 then
                    block_cnt_s <= block_cnt_r + 1;
                    word_cnt_s <= 0;
                    c_start <= '1';
                    n_state_s <= LD_CIN_AD;
                else
                    word_cnt_s <= word_cnt_r + 1;
                end if;
            when GEN_L_NTZ_AD =>
                en_li_out <= '1';
                n_state_s <= GEN_OI_HASH;
            when GEN_OI_HASH =>
                rotate_li <= '1';
                --rotate_oi <= '1';
                we_reg <= '1';
                sel_reg_din <= "01";
                sel_reg_xor <= '1';
                if word_cnt_r = BLOCK_WORDS_C - 1 then
                    n_state_s <= LD_CIN_AD;
                    word_cnt_s <= 0;
                    
                    c_start <= '1';
                else
                    word_cnt_s <= word_cnt_r + 1;
                end if;
            when LD_CIN_AD =>
                c_din_valid <= '1';
                if bdi_partial_r = '1' then
                    sel_bdi_offset <= '1';
                    rotate_lstar <= '1';
                end if;
                if word_cnt_r = BLOCK_WORDS_C - 1 then
                    word_cnt_s <= 0;
                    n_state_s <= GEN_AUTH;
                else
                    rotate_bdi_sr <= '1';
                    word_cnt_s <= word_cnt_r + 1;
                end if;
            when GEN_AUTH =>
                reg_cnt_s <= AUTH_ADDR_C;
                if c_dout_valid = '1' then
                    we_reg <= '1';
                    sel_reg_din <= "01";
                    if (word_cnt_r /= BLOCK_WORDS_C - 1) then
                        word_cnt_s <= word_cnt_r + 1;
                    end if;
                elsif c_done = '1' then
                    word_cnt_s <= 0;
                    if bdi_eot_r = '1' then
                        if bdi_eoi_r = '1' then
                            reg_cnt_s <= OFFSET_ADDR_C;
                            n_state_s <= LD_OFFSET_0;
                        else
                            n_state_s <= WAIT_INPUT;
                        end if;
                    else
                        n_state_s <= ABSORB_AD;
                    end if;
                end if;
            -- load message and generate PT/CT
            when LD_OFFSET_0 =>
                sel_sum <= "10";
                en_sum <= '1';
                we_reg <= '1';
                
                rotate_o0  <= '1';
                clr_bdi_partial <= '1';
                sel_bdi_size <= '1';
                en_bdi_size <= '1';
                sel_bdi_valid_bytes <= '1';
                en_bdi_valid_bytes <= '1';
                sel_bdi_pad_loc <= '1';
                en_bdi_pad_loc <= '1';
                if reg_cnt_r = OFFSET_ADDR_C then
                    sel_reg_din <= "10";
                else
                    sel_reg_din <= "00";
                end if;
                if word_cnt_r = BLOCK_WORDS_C - 1 then
                    word_cnt_s <= 0;
                    if bdi_eoi_r = '1' then
                        c_start <= '1';
                        sel_li_addr <= '1';
                        en_li_out <= '1';
                        reg_cnt_s <= OFFSET_ADDR_C;
                        n_state_s <= LD_TAG;
                    else
                        n_state_s <= ABSORB_MSG;
                    end if;
                else
                    word_cnt_s <= word_cnt_r + 1;
                end if;
            when ABSORB_MSG =>
                bdi_ready <= '1';
                if bdi_partial_s = '1' and bdi_valid = '1' and word_cnt_r < BLOCK_WORDS_C - 1 then
                    word_cnt_s <= word_cnt_r + 1;
                    en_bdi_size <= '1';
                    en_bdi_valid_bytes <= '1';
                    en_bdi_pad_loc <= '1';
                    en_bdi_partial <= '1';
                    sel_bdi <= "01";
                    en_bdi_eoi <= '1';
                    en_bdi_sr <= '1';
                    en_bdi_eot <= '1';
                    en_decrypt_in <= '1';
                    n_state_s <= PADD_MSG;
                elsif bdi_partial_s = '1' and bdi_valid = '1' and word_cnt_r = BLOCK_WORDS_C - 1 then
                    en_bdi_size <= '1';
                    en_bdi_valid_bytes <= '1';
                    en_bdi_pad_loc <= '1';
                    en_bdi_partial <= '1';
                    en_bdi_eoi <= '1';
                    en_bdi_sr <= '1';
                    en_bdi_eot <= '1';
                    en_decrypt_in <= '1';

                    sel_bdi <= "01";
                    
                    block_cnt_s <= block_cnt_r + 1;
                    word_cnt_s <= 0;
                    c_start <= '1';
                    n_state_s <= LD_CIN_MSG;
                elsif bdi_eot = '1' and bdi_valid = '1' and word_cnt_r < BLOCK_WORDS_C - 1 then
                    en_bdi_partial <= '1';
                    en_bdi_size <= '1';
                    en_bdi_valid_bytes <= '1';
                    en_bdi_pad_loc <= '1';
                    en_bdi_eoi <= '1';
                    en_bdi_sr <= '1';
                    en_bdi_eot <= '1';
                    sel_bdi <= "01";
                    word_cnt_s <= word_cnt_r + 1;
                    n_state_s <= PADD_MSG_10_WORD;
                elsif word_cnt_r = BLOCK_WORDS_C - 1 and bdi_valid = '1' then
                    block_cnt_s <= block_cnt_r + 1;
                    en_bdi_size <= '1';
                    en_bdi_valid_bytes <= '1';
                    en_bdi_pad_loc <= '1';
                    en_bdi_eoi <= '1';
                    en_bdi_sr <= '1';
                    en_bdi_eot <= '1';
                    en_decrypt_in <= '1';
                    word_cnt_s <= 0;
                    n_state_s <= GEN_L_NTZ_MSG;
                elsif bdi_valid = '1' then
                    en_bdi_size <= '1';
                    en_bdi_valid_bytes <= '1';
                    en_bdi_pad_loc <= '1';
                    en_bdi_sr <= '1';
                    word_cnt_s <= word_cnt_r + 1;
                end if;
            when PADD_MSG_10_WORD =>
                sel_bdi <= "01";
                en_bdi_sr <= '1';
                en_bdi_size <= '1';
                en_bdi_valid_bytes <= '1';
                en_bdi_pad_loc <= '1';
                if word_cnt_r = BLOCK_WORDS_C - 1 then
                    block_cnt_s <= block_cnt_r + 1;
                    word_cnt_s <= 0;
                    c_start <= '1';
                    n_state_s <= LD_CIN_MSG;
                else
                    word_cnt_s <= word_cnt_r + 1;
                    n_state_s <= PADD_MSG;
                end if;
            when PADD_MSG =>
                sel_bdi <= "10";
                en_bdi_sr <= '1';
                sel_bdi_size <= '1';
                sel_bdi_pad_loc <= '1';
                sel_bdi_valid_bytes <= '1';
                en_bdi_size <= '1';
                en_bdi_valid_bytes <= '1';
                en_bdi_pad_loc <= '1';
                if word_cnt_r = BLOCK_WORDS_C - 1 then
                    en_bdi_sr <= '1';
                    block_cnt_s <= block_cnt_r + 1;
                    word_cnt_s <= 0;
                    c_start <= '1';
                    n_state_s <= LD_CIN_MSG;
                else
                    word_cnt_s <= word_cnt_r + 1;
                end if;
            when GEN_L_NTZ_MSG =>
                en_li_out <= '1';
                n_state_s <= GEN_OI_MSG;
                reg_cnt_s <= OFFSET_ADDR_C;
            when GEN_OI_MSG =>
                rotate_li <= '1';
                we_reg <= '1';
                sel_reg_din <= "01";
                sel_reg_xor <= '1';
                if word_cnt_r = BLOCK_WORDS_C - 1 then
                    n_state_s <= LD_CIN_MSG;
                    sel_decrypt <= '1';
                    word_cnt_s <= 0;
                    c_start <= '1';
                else
                    word_cnt_s <= word_cnt_r + 1;
                end if;
            when LD_CIN_MSG =>
                c_din_valid <= '1';
                --rotate_oi <= '1';
                if decrypt_in_r = '0' then
                    en_sum <= '1';
                end if;
                sel_decrypt <= '1';
                if bdi_partial_r = '1' then
                   -- sel_tag_offset(0) <= '1';
                    sel_bdi_offset <= '1';
                    rotate_lstar <= '1';
                    sel_cin <= "11";
                    sel_decrypt <= '0';
                end if;
                if word_cnt_r = BLOCK_WORDS_C - 1 then
                    n_state_s <= GEN_CT;
                    rotate_bdi_sr <= '1';
                    word_cnt_s <= 0;
                elsif decrypt_in_r = '1' and bdi_partial_r = '0' then
                    --if c_done_mm = '1' then
                        rotate_bdi_sr <= '1';
                        word_cnt_s <= word_cnt_r + 1;
                    --end if;
                else 
                    rotate_bdi_sr <= '1';
                    word_cnt_s <= word_cnt_r + 1;
                end if;
            when GEN_CT =>
                if bdi_partial_r = '1' then
                    sel_decrypt <= '0'; 
                else
                    sel_decrypt <= '1';
                end if;
                if c_dout_valid = '1' then
                    en_c_dout_sr <= '1';
                elsif c_done = '1' then
                    reg_cnt_s <= OFFSET_ADDR_C;
                    n_state_s <= OUTPUT_DATA;
                end if;
            when OUTPUT_DATA =>
                bdo_valid_s <= '1';
                if bdi_size_s = "100" then
                    bdo_valid_bytes_s <= x"F";
                elsif bdi_size_s = "011" then
                    bdo_valid_bytes_s <= x"E";
                elsif bdi_size_s = "010" then
                    bdo_valid_bytes_s <= x"C";
                elsif bdi_size_s = "001" then
                    bdo_valid_bytes_s <= x"8";
                else 
                    bdo_valid_bytes_s <= x"0";
                    bdo_valid_s <= '0';
                end if;
                if decrypt_in_r = '1' then
                    en_sum <= '1';
                    sel_sum <= "01";
                end if;
                
                if bdi_partial_r = '1' then
                    sel_bdo <= '1';      
                    sel_sum <= "11"; 
                end if;
                
                
                if word_cnt_r = BLOCK_WORDS_C - 1 and (bdo_ready = '1' or bdi_size_s = "000") then
                    en_bdi_size <= '1';
                    en_bdi_valid_bytes <= '1';
                    en_bdi_pad_loc <= '1';
                    if bdi_eot_r = '0' then -- do not clear bdi_info registers if this is last message
                        sel_bdi_size <= '1';
                        sel_bdi_pad_loc <= '1';
                        sel_bdi_valid_bytes <= '1';
                    end if;
                    word_cnt_s <= 0;
                    rotate_c_dout_sr <= '1';
                    if bdi_eot_r = '1' then
                        n_state_s <= LD_TAG;
                        reg_cnt_s <= OFFSET_ADDR_C;
                        sel_li_addr <= '1';
                        en_li_out <= '1';
                        c_start <= '1';
                    elsif bdo_ready = '1' then
                        n_state_s <= ABSORB_MSG;
                    end if;
                elsif bdo_ready = '1' or bdi_size_s = "000" then
                    en_bdi_size <= '1';
                    en_bdi_valid_bytes <= '1';
                    en_bdi_pad_loc <= '1';
                    if bdi_eot_r = '0' then -- do not clear bdi_info registers if this is last message
                        sel_bdi_size <= '1';
                        sel_bdi_pad_loc <= '1';
                        sel_bdi_valid_bytes <= '1';
                    end if;
                    rotate_bdi_sr <= '1';
                    rotate_c_dout_sr <= '1';
                    word_cnt_s <= word_cnt_r + 1;
                end if;            
            when LD_TAG =>
                if bdi_partial_r = '1' then
                  --  sel_tag_offset(0) <= '1';
                    sel_bdi_offset <= '1';
                    rotate_lstar <= '1';
                end if;
                sel_tag_offset(0) <= '1'; -- L$ 
                sel_tag_offset(1) <= '1'; -- sum_r
                sel_cin <= "11";
                c_din_valid <= '1';
                rotate_li <= '1';
                rotate_sum <= '1';
                if word_cnt_r = BLOCK_WORDS_C - 1 then
                    n_state_s <= GEN_TAG;
                    word_cnt_s <= 0;
                else
                    word_cnt_s <= word_cnt_r + 1;
                end if;
            when GEN_TAG =>
                if c_dout_valid = '1' then
                    en_c_dout_sr <= '1';
                elsif c_done = '1' then
                    reg_cnt_s <= AUTH_ADDR_C;
                    if decrypt_in_r = '1' then
                        n_state_s <= VERIFY_TAG;
                    else
                        n_state_s <= OUTPUT_TAG;
                    end if;
                end if;
            when VERIFY_TAG =>
                sel_bdo_offset <= '1';
                bdi_ready <= '1';
                rotate_c_dout_sr <= '1';
                if tag_match = '0' then
                    n_state_s <= TAG_MATCH_FAILURE;
                elsif word_cnt_r = BLOCK_WORDS_C - 1 and bdi_valid = '1' then
                    n_state_s <= TAG_MATCH_SUCCESS;
                elsif bdi_valid = '1' then
                    word_cnt_s <= word_cnt_r + 1;
                end if;
            when TAG_MATCH_SUCCESS =>
                msg_auth_valid <= '1';
                msg_auth <= '1';
                if msg_auth_ready = '1' then
                    n_state_s <= IDLE;
                end if;
            when TAG_MATCH_FAILURE =>
                msg_auth_valid <= '1';
                msg_auth <= '0';
                if msg_auth_ready = '1' then
                    n_state_s <= IDLE;
                end if;
            when OUTPUT_TAG =>
                bdo_valid_s <= '1';
                sel_bdo_offset <= '1';
                
                bdo_valid_bytes_s <= x"F";
                if word_cnt_r = BLOCK_WORDS_C - 1 and bdo_ready = '1' then
                    end_of_block <= '1';
                    rotate_c_dout_sr <= '1';
               --     rotate_oi <= '1';
                    n_state_s <= IDLE;
                elsif bdo_ready = '1' then
                    rotate_c_dout_sr <= '1';
                  --  rotate_oi <= '1';
                    word_cnt_s <= word_cnt_r + 1;
                end if;
                when others => null;
        end case;
    end process FSM_control_path;

    -- datapath
    registers: process(clk)
    begin
        if rising_edge(clk) then
            --bdi shift register
            if en_bdi_sr = '1' then
                bdi_sr <= bdi_sr(CCW to DBLK_SIZE - 1) & bdi_mux;
            elsif rotate_bdi_sr = '1' then
                bdi_sr <= bdi_sr(CCW to DBLK_SIZE - 1) & bdi_sr(0 to CCW - 1);
            end if;
            if en_li_in = '1' then
                li_in_r <= li_in_r(CCW to DBLK_SIZE - 1) & li_in_mux;
            end if;
            if en_li_out = '1' then
                li_out_r <= li_dout;
            elsif rotate_li = '1' then
                li_out_r <= li_out_r(CCW to DBLK_SIZE - 1) & li_out_r(0 to CCW - 1);
            end if;
            -- L*
            if en_lstar = '1' then
                lstar_r <= lstar_r(CCW to DBLK_SIZE - 1) & c_dout;
            elsif rotate_lstar = '1' then
                lstar_r <= lstar_r(CCW to DBLK_SIZE - 1) & lstar_r(0 to CCW - 1);
            end if;
           
            -- Checksum
            if en_sum = '1' then
                sum_r <= sum_r(CCW to DBLK_SIZE - 1) & sum_mux;
            elsif rotate_sum = '1' then
                sum_r <= sum_r(CCW to DBLK_SIZE - 1) & sum_r(0 to CCW - 1);
            end if;
           
            -- BDI Eoi
            if en_bdi_eoi = '1' then
                bdi_eoi_r <= bdi_eoi;
            elsif clr_bdi_eoi = '1' then
                bdi_eoi_r <= '0';
            end if;
            -- BDI EOT
            if en_bdi_eot = '1' then
                bdi_eot_r <= bdi_eot;
            elsif clr_bdi_eot = '1' then
                bdi_eot_r <= '0';
            end if;
            -- decrypt_in
            if en_decrypt_in = '1' then
                decrypt_in_r <= decrypt_in;
            elsif clr_decrypt_in = '1' then
                decrypt_in_r <= '0';
            end if;
            -- bdi partial
            if en_bdi_partial = '1' then
                bdi_partial_r <= bdi_partial_s or bdi_eot;
            elsif clr_bdi_partial = '1' then
                bdi_partial_r <= '0';
            end if;
            -- c_dout shift register
            if en_c_dout_sr = '1' then
                c_dout_r <= c_dout_r(CCW to DBLK_SIZE - 1) & c_dout;
            elsif rotate_c_dout_sr = '1' then
                c_dout_r <= c_dout_r(CCW to DBLK_SIZE -1) & c_dout_r(0 to CCW - 1);
            end if;
            -- bdi size
            if en_bdi_size = '1' then
                bdi_size_r <= bdi_size_r(8 downto 0) & bdi_size_mux;
            end if;
            if en_bdi_pad_loc = '1' then
                bdi_pad_loc_r <= bdi_pad_loc_r(11 downto 0) & bdi_pad_loc_mux;
            end if;
            if en_bdi_valid_bytes = '1' then
                bdi_valid_bytes_r <= bdi_valid_bytes_r(11  downto 0) & bdi_valid_bytes_mux;
            end if;
        end if;
    end process registers;

    counters: process(clk)
    begin
        if rising_edge(clk) then
            if rst = '1'then
                word_cnt_r <= 0;
                block_cnt_r <= 0;
                l_cnt_r <= 0;
                reg_cnt_r <= 0;
            else
                block_cnt_r <= block_cnt_s;
                l_cnt_r <= l_cnt_s;
                word_cnt_r <= word_cnt_s;
                bottom_r <= bottom_s;
                reg_cnt_r <= reg_cnt_s;
            end if;
        end if;
    end process counters;


    li_s <= li_out_r(0 to CCW - 1);
    bdi_s <= bdi_sr(0 to CCW - 1);
    sum_s <= sum_r(0 to CCW - 1);
    lstar_s <= lstar_r(0 to CCW - 1);
    c_dout_s <= c_dout_r(0 to CCW - 1);
    
    bdi_size_s <= bdi_size_r(11 downto 9);
    bdi_valid_bytes_s <= bdi_valid_bytes_r(15 downto 12);
    bdi_pad_loc_s <= bdi_pad_loc_r(15 downto 12);

    bdi_valid_bytes_mux <= bdi_valid_bytes when sel_bdi_valid_bytes = '0' else (others => '0');
    bdi_pad_loc_mux <= bdi_pad_loc when sel_bdi_pad_loc = '0' else (others => '0');
    bdi_size_mux <= bdi_size when sel_bdi_size = '0' else (others => '0');

    with sel_bdi select bdi_mux <=
        bdi when "00",
        padd(bdi, bdi_valid_bytes, bdi_pad_loc) when "01",
        (others => '0') when "10",
        (others => '-') when others;
    
    bdi_offset_mux <= reg_dout xor lstar_s when sel_bdi_offset = '1' else 
                      reg_dout;

    bdi_partial_s <= or_reduce(bdi_pad_loc);

    bdo_valid_bytes <= bdo_valid_bytes_s;
    
    bdo_valid <= bdo_valid_s;

    c_decrypt <= decrypt_in_r when sel_decrypt = '1' else '0';

    tag_offset_mux_l(0) <= bdi_offset_mux;
    --tag_offset_mux_r(0) <= tag_offset_mux_l(0) xor lstar_s;
    tag_offset_mux_r(0) <= tag_offset_mux_l(0) xor li_s;
    tag_offset_mux_r(1) <= tag_offset_mux_l(1) xor sum_s;
    tag_offset_mux_gen: for i in 0 to 1 generate
        tag_offset_mux_l(i+1) <= tag_offset_mux_r(i) when sel_tag_offset(i) = '1' else tag_offset_mux_l(i);
    end generate tag_offset_mux_gen;



    with sel_nonce select nonce_mux <=
        bdi_s when "00",
        NONCE_C when "01",
        bdi_s(0 to CCW - 7) & "000000" when "10",
        (others => '-') when others;

    with sel_cin select cin_mux <=
        bdi_s xor bdi_offset_mux when "00",
        nonce_mux when "01",
        (others => '0') when "10",
        tag_offset_mux_l(2) when others;
    
  --  c_din <= cin_r;


    i_stretch_hash : entity work.StretchHash
        port map(
            clk => clk,
            ktop => c_dout,
            ld => en_stretchhash,
            rotate => rot_stretchhash,
            rotate_o0 => rotate_o0,
            offset => o0_s
        );
    -- calculate offset values
    

    -- calculate L values


    i_li_ram : entity work.SPDRam
    generic map(
        DataWidth => DBLK_SIZE,
        AddrWidth => 4
    )
    port map(
        clk     => clk,
        wen     => we_li,
        addr    => li_addr,
        din     => li_din,
        dout    => li_dout
    );
    

    li_din <= dbl_s;
    li_addr <= std_logic_vector(to_unsigned(l_cnt_r, li_addr'length)) when sel_li_addr = '1' else
               std_logic_vector(to_unsigned(ntz(block_cnt_r) + 1, li_addr'length));
    dbl_msb_s <= li_in_r(0);
    
    
    li_in_mux <= c_dout when sel_li = '1' else li_s;

    dbl_s <= li_in_r(1 to DBLK_SIZE - 8) 
            & (dbl_msb_s xor li_in_r(DBLK_SIZE - 7))
            & li_in_r(DBLK_SIZE - 6 to DBLK_SIZE - 3)
            & (dbl_msb_s xor li_in_r(DBLK_SIZE - 2))
            & (dbl_msb_s xor li_in_r(DBLK_SIZE - 1)) 
            & dbl_msb_s; 


    i_reg_ram : entity work.SPDRam
    generic map(
        DataWidth => CCW,
        AddrWidth => 4
    )
    port map(
        clk     => clk,
        wen     => we_reg,
        addr    => reg_addr,
        din     => reg_din,
        dout    => reg_dout
    );
    
     with sel_reg_din select reg_din <=
        reg_xor_mux xor reg_dout when "01",
        o0_s when "10",
        (others => '0') when "00",
        (others => '-') when others; 
    reg_xor_mux <= li_s when sel_reg_xor = '1' else c_dout;
    reg_addr <= std_logic_vector(to_unsigned(reg_cnt_r, reg_addr'length - 2)) 
                & std_logic_vector(to_unsigned(word_cnt_r, 2));
    -- sum
    with sel_sum select sum_mux <=
        sum_s xor bdi_s when "00",
        sum_s xor c_dout_s xor bdo_offset_mux when "01",
        (others => '0') when "10",
        sum_s xor padd((c_dout_s xor bdi_s), bdi_valid_bytes_s, bdi_pad_loc_s) when others;
        -- hash
    -- bdo mux
     bdo_offset_mux <= reg_dout;--auth_s when sel_bdo_offset = '1' else oi_s;

    bdo <= c_dout_s xor bdo_offset_mux when sel_bdo = '0' else
           (c_dout_s xor bdi_s);
    tag_match <= '1' when ((c_dout_s xor bdo_offset_mux) = bdi) else '0';

end behavioral;
