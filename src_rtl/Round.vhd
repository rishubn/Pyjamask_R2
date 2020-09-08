--------------------------------------------------------------------------------
--! @file       Round.vhd
--! @brief      Executes 1 pyjamask round
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
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.design_pkg.all;
entity Round is
    port (
        clk : in std_logic;
        mi : in std_logic_vector(0 to CCW - 1);
        co : out std_logic_vector(0 to CCW - 1);
        sel_row : in std_logic_vector(1 downto 0);
        sel_inv : in std_logic;
        en_subbytes : in std_logic;
        shift_in : in std_logic;
        rkey : in std_logic_vector(0 to CCSW - 1)
    );
end entity Round;

architecture rtl of Round is
    signal add_round_key_s : std_logic_vector(0 to CCW - 1);
    signal in_sr : std_logic_vector(0 to NUM_MATRIX_ROWS*CCW - 1);
    signal in_mux : std_logic_vector(0 to CCW - 1);
    signal out_mux : std_logic_vector(0 to CCW - 1);

    signal add_key_s : std_logic_vector(0 to CCW - 1);
    signal add_key_mux : std_logic_vector(0 to CCW - 1);
    signal subbytes_in : std_logic_vector(0 to CCW - 1);
    signal mi_r0 : std_logic_vector(0 to CCW - 1);
    signal mi_r1 : std_logic_vector(0 to CCW - 1);
    signal mi_r2 : std_logic_vector(0 to CCW - 1);
    signal mi_r3 : std_logic_vector(0 to CCW - 1);



    signal sb_row : std_logic_vector(0 to CCW - 1);
    
    type sb_array is array(0 to 31) of std_logic_vector(0 to 3);
    type addr_array is array(0 to 31) of std_logic_vector(0 to 4);
    signal addr_s : addr_array;

    signal subbytes_out : sb_array;
    signal sb_r0 : std_logic_vector(0 to CCW - 1);
    signal sb_r1 : std_logic_vector(0 to CCW - 1);
    signal sb_r2 : std_logic_vector(0 to CCW - 1);
    signal sb_r3 : std_logic_vector(0 to CCW - 1);

    signal sb_r0_r : std_logic_vector(0 to CCW - 1);
    signal sb_r1_r : std_logic_vector(0 to CCW - 1);
    signal sb_r2_r : std_logic_vector(0 to CCW - 1);
    signal sb_r3_r : std_logic_vector(0 to CCW - 1);
begin
    

    shift_register: process(clk)
    begin
        if rising_edge(clk) then
            if shift_in = '1' then
                in_sr <= in_sr(CCW to 4*CCW - 1) & in_mux; -- Add round key
            end if;
            if en_subbytes = '1' then
                sb_r0_r <= sb_r0;
                sb_r1_r <= sb_r1;
                sb_r2_r <= sb_r2;
                sb_r3_r <= sb_r3;
            end if;
        end if;
    end process shift_register;

   -- shift_in_s <= cmm_dout_valid_s when sel_inv = '1' else shift_in;

    in_mux <= mi when sel_inv = '1' else add_round_key_s;

    mi_r0 <= in_sr(0 to CCW - 1);
    mi_r1 <= in_sr(CCW to 2*CCW - 1);
    mi_r2 <= in_sr(2*CCW to 3*CCW - 1);
    mi_r3 <= in_sr(3*CCW to 4*CCW - 1);

    
    subbytes_rom_gen: for i in 0 to 31 generate
        addr_s(i) <= sel_inv & mi_r0(i) & mi_r1(i) & mi_r2(i) & mi_r3(i);

        i_subbytes_rom : entity work.SubbytesRom
        port map(
            dout => subbytes_out(i),
            addr => addr_s(i)
        );
    end generate subbytes_rom_gen;

    -- reorder the output of subbytes back to 32-bit rows
    subbytes_out_gen: for i in 0 to 31 generate
        sb_r0(i) <= subbytes_out(i)(0);
        sb_r1(i) <= subbytes_out(i)(1);
        sb_r2(i) <= subbytes_out(i)(2);
        sb_r3(i) <= subbytes_out(i)(3);
    end generate subbytes_out_gen;
    
    with sel_row select sb_row <=
        sb_r0_r when "00",
        sb_r1_r when "01",
        sb_r2_r when "10",
        sb_r3_r when others;


    -- mix rows
    add_key_mux <= sb_row when sel_inv = '1' else mi;

    add_round_key_s <= add_key_mux xor rkey;


    co <= add_round_key_s when sel_inv = '1' else sb_row;
end architecture rtl;
