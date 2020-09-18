--------------------------------------------------------------------------------
--! @file       PyjamaskEncDec.vhd
--! @brief      Pyjamask Block Cipher
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
--use work.NIST_LWAPI_pkg.all;
use work.design_pkg.all;
entity PyjamaskEncDec is
    port (
        clk : in std_logic;
        rst : in std_logic;
        -- key signals
        key   : in std_logic_vector(0 to CCSW - 1);
        key_ready : out std_logic;
        key_valid : in std_logic;
        -- input flags
        init  : in std_logic;
        start : in std_logic;
        decrypt : in std_logic;
        din_valid : in std_logic;
        -- status flags
        done      : out std_logic;
        done_init : out std_logic;
        --done_mm : out std_logic;
        ready     : out std_logic;
        dout_valid : out std_logic;
        -- data
        din   : in std_logic_vector(0 to CCW - 1);
        dout : out std_logic_vector(0 to CCW - 1)
    );
end PyjamaskEncDec;
architecture behavioral of PyjamaskEncDec is


    type state_t is (RESET, IDLE, ABSORB_KEY, WRITE_RKEY, ABSORB_PT, ABSORB_CT, PROCESS_PT, PROCESS_CT);--, OUTPUT_MSG);
    signal n_state_reg, state_reg : state_t;



    signal sel_key : std_logic; -- ki key select
    signal ki : std_logic_vector(0 to CCSW - 1); 
    signal ko : std_logic_vector(0 to CCSW - 1);
    signal kr_din : std_logic_vector(0 to CCSW - 1);
    signal kr_addr : std_logic_vector(KEY_ADDR_BITS_C - 1 downto 0);

    signal we_rkey : std_logic;

    signal rnd_shift_rows : std_logic;
    signal round_cnt_s, round_cnt_r : integer range 0 to NB_ROUNDS_KS_C;
    signal mm_word_cnt_s, mm_word_cnt_r : integer range 0 to BLOCK_WORDS_C;
    signal word_cnt_s, word_cnt_r : integer range 0 to BLOCK_WORDS_C;
    signal word_s, mm_word_s : std_logic_vector(log2_ceil(BLOCK_WORDS_C) - 1 downto 0);
    signal round_s : std_logic_vector(log2_ceil(NB_ROUNDS_KS_C) - 1 downto 0);
    signal ku_round_s : std_logic_vector(log2_ceil(NB_ROUNDS_KS_C) - 1 downto 0);
    signal en_rkey : std_logic;
    signal kshift_in : std_logic;
    signal en_krow : std_logic;
    signal rkey_reg : std_logic_vector(0 to CCSW - 1);
    signal kr_out : std_logic_vector(0 to CCSW - 1);

    signal done_init_s, done_init_r : std_logic;
    signal done_s, done_r : std_logic;
    signal din_mux : std_logic_vector(0 to CCW - 1);
    signal encout_mux : std_logic_vector(0 to CCW - 1);

    signal round_out : std_logic_vector(0 to CCW - 1);

    signal decrypt_reg, en_decrypt : std_logic;

    signal sel_in : std_logic;
    signal en_in : std_logic;
    signal sel_encout : std_logic;
    signal sel_decrypt : std_logic;
    signal sel_krdin : std_logic;
    signal en_encr : std_logic;
    signal kcmm_done : std_logic;
    signal cmm_din_valid_s : std_logic;
    signal en_subbytes : std_logic;
    signal shift_in : std_logic;
    signal col_diff_out_s : std_logic_vector(0 to CCSW - 1);

    signal mm_in_mux : std_logic_vector(0 to CCW - 1);
    signal sel_mm_in : std_logic_vector(1 downto 0);
    signal mm_dout : std_logic_vector(0 to CCW - 1);
    signal inv_matrix_mux : std_logic_vector(0 to CCW - 1);
    signal matrix_mux : std_logic_vector(0 to CCW - 1);
    signal key_matrix_mux : std_logic_vector(0 to CCW - 1);
    signal mm_decrypt_mux : std_logic_vector(0 to CCW - 1);

    signal sel_key_matrix : std_logic;
    signal mm_din_valid : std_logic;
    signal mm_din_valid_s : std_logic;
    signal mm_dout_valid : std_logic;

    signal dout_r : std_logic_vector(0 to DBLK_SIZE - 1);
    signal en_dout : std_logic;
    signal rot_dout : std_logic;

    signal dout_mux : std_logic_vector(0 to CCW - 1);
begin


    FSM_state_register : process(clk)
    begin
        if rising_edge(clk) then
            if (rst = '1') then
                state_reg <= RESET;
                round_cnt_r <= 0;
                word_cnt_r <= 0;
            else
                state_reg <= n_state_reg;
                round_cnt_r <= round_cnt_s;
                word_cnt_r <= word_cnt_s;
                mm_word_cnt_r <= mm_word_cnt_s;
            end if;
        end if;
    end process FSM_state_register;

    FSM_control_path : process(all)
    begin
        --- Default values
        n_state_reg <= state_reg;
        round_cnt_s <= round_cnt_r;
        word_cnt_s <= word_cnt_r;
        mm_word_cnt_s <= mm_word_cnt_r;
        ready <= '0';
        sel_key <= '0';
        en_rkey <= '0';
        done_init_s <= '0';
        we_rkey <= '0';
        sel_in <= '0';
        en_in <= '0';
        sel_decrypt <= '0';
        done_s <= '0';
        done_init_s <= '0';
        sel_encout <= '0';
        sel_krdin <= '0';
        en_encr <= '0';
        en_decrypt <= '0';
        en_krow <= '0';
        rnd_shift_rows <= '0';
        en_subbytes <= '0';
        kshift_in <= '0';
        key_ready <= '0';
        shift_in <= '0';
        dout_valid <= '0';
        --mm_din_valid <= '0';
        sel_key_matrix <= '0';
        sel_mm_in <= "00";
        en_dout <= '0';
        rot_dout <= '0';
        mm_din_valid_s <= '0';
        case state_reg is
            when RESET =>
                round_cnt_s <= 0;
                word_cnt_s <= 0;
                n_state_reg <= IDLE;
            when IDLE =>
                ready <= '1';
                round_cnt_s <= 0;
                mm_word_cnt_s <= 0;
                word_cnt_s <= 0;
                if init = '1' then
                    n_state_reg <= ABSORB_KEY;
                elsif start = '1' then
                    if decrypt = '1' then
                        round_cnt_s <= NB_ROUNDS_KS_C - 1;
                        mm_din_valid_s <= '1';
                        n_state_reg <= ABSORB_CT;
                    else
                        n_state_reg <= ABSORB_PT;
                        round_cnt_s <= 0;
                    end if;
                    en_decrypt <= '1';
                    --n_state_reg <= ABSORB_MSG;
                end if;
            when ABSORB_KEY =>
                sel_krdin <= '1';
                sel_key <= '1';
                key_ready <= '1';
                if word_cnt_r = BLOCK_WORDS_C then
                    n_state_reg <= WRITE_RKEY;
                    we_rkey <= '0';
                    kshift_in <= '0';
                    sel_key <= '0';
                    sel_krdin <= '0';
                    if decrypt = '1' then
                        round_cnt_s <= round_cnt_r - 1;
                    else
                        round_cnt_s <= round_cnt_r + 1;
                    end if;
                    word_cnt_s <= 0;
                    en_krow <= '1';
                    mm_din_valid_s <= '1';
                elsif key_valid = '1' then
                    we_rkey <= '1'; -- only write/shift on valid keys
                    kshift_in <= '1';
                    word_cnt_s <= word_cnt_r + 1;
                end if;
            when WRITE_RKEY =>
                sel_mm_in <= "01";
                sel_key_matrix <= '1';
                we_rkey <= '1';
                kshift_in <= '1';
                if round_cnt_r = NB_ROUNDS_KS_C then
                    word_cnt_s <= 0;
                    we_rkey <= '0';
                    done_init_s <= '1';
                    n_state_reg <= IDLE;
                elsif word_cnt_r = BLOCK_WORDS_C then
                    round_cnt_s <= round_cnt_r + 1;
                    word_cnt_s <= 0;
                    mm_din_valid_s <= '1';
                    we_rkey <= '0';
                    kshift_in <= '0';
                    en_krow <= '1';
                elsif word_cnt_r = 0 and mm_dout_valid = '0' then
                    we_rkey <= '0';
                    kshift_in <= '0';
                else
                    word_cnt_s <= word_cnt_r + 1;
                end if;
            when ABSORB_PT =>
                sel_in <= '1';
                if word_cnt_r = BLOCK_WORDS_C then
                    word_cnt_s <= 0;
                    en_subbytes <= '1';
                        mm_din_valid_s <= '1';
                        round_cnt_s <= round_cnt_r + 1;
                    n_state_reg <= PROCESS_PT;
                elsif din_valid = '1' then
                    word_cnt_s <= word_cnt_r + 1;
                    --if decrypt = '1' then
                       -- n_state_reg <= WAIT_MM;
                     -- if mm_word_cnt_r < BLOCK_WORDS_C -1 then
                      --  mm_din_valid_s <= '1';
                       -- mm_word_cnt_s <= mm_word_cnt_r + 1;
                     -- end if;
                     -- if mm_dout_valid = '1' then
                      --  shift_in <= '1';
                      --end if;
                    --else
                        shift_in <= '1';
                   -- end if;
                end if;
            when ABSORB_CT =>
              --sel_in <= '1';
              --sel_decin <= '1';
              sel_mm_in <= "10";
              if word_cnt_r = BLOCK_WORDS_C then
                    word_cnt_s <= 0;
                    --en_subbytes <= '1';
                    round_cnt_s <= round_cnt_r - 1;
                    mm_word_cnt_s <= 0;
                    shift_in <= '1';
                    n_state_reg <= PROCESS_CT;
              elsif din_valid = '1' then
                word_cnt_s <= word_cnt_r + 1;
                if mm_word_cnt_r < BLOCK_WORDS_C -1 then
                    mm_din_valid_s <= '1';
                    mm_word_cnt_s <= mm_word_cnt_r + 1;
                end if;
                if mm_dout_valid = '1' then
                        shift_in <= '1';
                end if;
              end if;
            when PROCESS_CT =>
              if (round_cnt_r = 0) then
                dout_valid <= '1';
                sel_encout <= '0';
                if word_cnt_r = BLOCK_WORDS_C - 1 then
                  done_s <= '1';
                  n_state_reg <= IDLE;
                end if;
              end if;
              if word_cnt_r = 0 and mm_din_valid = '0' then
                en_subbytes <= '1';
                mm_din_valid_s <= '1';
                --rnd_shift_rows <= '1';
              elsif word_cnt_r = 0 then
                word_cnt_s <= word_cnt_r + 1;
                mm_word_cnt_s <= mm_word_cnt_r + 1;
                mm_din_valid_s <= '1';
                rnd_shift_rows <= '1';
              elsif mm_word_cnt_r = BLOCK_WORDS_C then
                round_cnt_s <= round_cnt_r - 1;
                word_cnt_s <= 0;
                mm_word_cnt_s <= 0;
                shift_in <= '1';
              else
                if mm_word_cnt_r > 0 then
                  shift_in <= '1';
                end if;
                mm_word_cnt_s <= mm_word_cnt_r + 1;
                if word_cnt_r < BLOCK_WORDS_C then
                  if word_cnt_r < BLOCK_WORDS_C - 1 then
                    mm_din_valid_s <= '1';
                  end if;
                  rnd_shift_rows <= '1';
                  word_cnt_s <= word_cnt_r + 1;
                end if;
              end if;
            when PROCESS_PT =>
                if mm_word_cnt_r < BLOCK_WORDS_C -1 then
                  mm_din_valid_s <= '1';
                end if;
                rnd_shift_rows <= '1';
                if (round_cnt_r = NB_ROUNDS_KS_C -1 and (mm_word_cnt_r > 0)) then
                  dout_valid <= '1';
                  sel_encout <= '1';--not decrypt;
                  if word_cnt_r = BLOCK_WORDS_C - 1 then
                    done_s <= '1';
                    n_state_reg <= IDLE;
                  end if;
                end if;
             --   if (round_cnt_r = NB_ROUNDS_KS_C) then
             --       n_state_reg <= OUTPUT_MSG;
            --        word_cnt_s <= 0;
             --       mm_word_cnt_s <= 0;
                if word_cnt_r = BLOCK_WORDS_C then
                    --if decrypt = '1' then
                  --      round_cnt_s <= round_cnt_r - 1;
                 --   else
                        round_cnt_s <= round_cnt_r + 1;
                  --  end if;
                    word_cnt_s <= 0;
                    mm_word_cnt_s <= 0;
                    en_subbytes <= '1';
                    rnd_shift_rows <= '0';
                    mm_din_valid_s <= '1';
                elsif mm_dout_valid = '1' then
                    shift_in <= '1';
                    word_cnt_s <= word_cnt_r + 1;
                    if mm_word_cnt_r < BLOCK_WORDS_C then
                      mm_word_cnt_s <= mm_word_cnt_r + 1;
                    end if;
                elsif mm_word_cnt_r < BLOCK_WORDS_C then
                    mm_word_cnt_s <= mm_word_cnt_r + 1;
                end if;
           -- when OUTPUT_MSG =>
         --       dout_valid <= '1';
         --       sel_encout <= not decrypt;
         --       rot_dout <= '1';
         --       if word_cnt_r = BLOCK_WORDS_C-1 then
         --           done_s <= '1';
         --           n_state_reg <= IDLE;
         --       else
           --         word_cnt_s <= word_cnt_r + 1;
           --         rnd_shift_rows <= '1';
           --     end if;
        end case;

    end process FSM_control_path;

    -- datapath
    registers: process(clk)
    begin
        if rising_edge(clk) then
            if en_rkey = '1' then
               -- rkey_reg <= kr_out;
            end if;
            done_init_r <= done_init_s;
            done_r <= done_s;
            mm_din_valid <= mm_din_valid_s;
            if en_decrypt = '1' then
                decrypt_reg <= decrypt;
            end if;
            if en_dout = '1' then
                dout_r <=  dout_r(CCW to DBLK_SIZE - 1) & dout_mux;
            elsif rot_dout = '1' then
                dout_r <= dout_r(CCW to DBLK_SIZE - 1) & dout_r(0 to CCW - 1);
            end if;
        end if;
    end process registers;

    -- done outputs
    done <= done_r;
    done_init <= done_init_r;

    i_key_ram : entity work.SPDRam
    generic map(
        DataWidth => CCSW,
        AddrWidth => KEY_ADDR_BITS_C
    )
    port map(
        clk     => clk,
        wen     => we_rkey,
        addr    => kr_addr,
        din     => kr_din,
        dout    => kr_out
    );

    i_key_update : entity work.KeyUpdate
        port map(
            clk => clk,
            shift_in => kshift_in,
            sel_row => word_s,
            en_col_diff => en_krow,
            key_input => ki,
            key_output => ko,
            col_diff_out => col_diff_out_s,
            const_add_in => mm_dout,
            round_cnt => ku_round_s -- key update round counter is one behind address counter
        );

    i_round : entity work.Round
        port map(
            clk => clk,
            mi => din_mux,
            co => round_out,
            shift_rows => rnd_shift_rows,
            sel_inv => decrypt_reg,
            shift_in => shift_in,
            en_subbytes => en_subbytes,
            rkey => kr_out
        );
        i_matrix_multiply : entity work.MatrixMultiply
            generic map(
                DataWidth => CCW,
                PipelineDepth => 2
            )
            port map(
                clk => clk,
               -- rst => rst,
                --data signals
                din => mm_in_mux,
                din_valid => mm_din_valid,
                matrix => key_matrix_mux,
                dout => mm_dout,
                dout_valid => mm_dout_valid
            );

    with mm_word_s select matrix_mux <=
        COL_M0 when "00",
        COL_M1 when "01",
        COL_M2 when "10",
        COL_M3 when others;

    with mm_word_s select inv_matrix_mux <=
        COL_INV_M0 when "00",
        COL_INV_M1 when "01",
        COL_INV_M2 when "10",
        COL_INV_M3 when others;
    
    mm_decrypt_mux <= inv_matrix_mux when decrypt = '1' else matrix_mux;
    key_matrix_mux <= COL_MK when sel_key_matrix = '1' else mm_decrypt_mux;

    with sel_mm_in select mm_in_mux <=
        round_out when "00",
        col_diff_out_s when "01",
        din xor kr_out when "10",
        (others => '-') when others;

    din_mux <= din when sel_in = '1' else encout_mux;
    encout_mux <= mm_dout xor kr_out when sel_encout = '1' else mm_dout;
    dout_mux <= round_out when decrypt = '1' else encout_mux;
    dout <= dout_mux; --dout_r(0 to CCW - 1);

    round_s <= std_logic_vector(to_unsigned(round_cnt_r, log2_ceil(NB_ROUNDS_KS_C)));
    ku_round_s <= (others => '0') when (round_cnt_r <= 1) else
                  std_logic_vector(to_unsigned(round_cnt_r - 1, log2_ceil(NB_ROUNDS_KS_C)));
    word_s <= std_logic_vector(to_unsigned(word_cnt_r, log2_ceil(BLOCK_WORDS_C)));
    mm_word_s <= std_logic_vector(to_unsigned(mm_word_cnt_r, log2_ceil(BLOCK_WORDS_C)));
    ki <= key when sel_key = '1' else ko;
    kr_addr <= round_s & word_s;
    kr_din <= key when sel_krdin = '1' else ko;

end architecture behavioral;
