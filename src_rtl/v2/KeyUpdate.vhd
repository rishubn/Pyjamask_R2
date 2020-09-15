--------------------------------------------------------------------------------
--! @file       KeyUpdate.vhd
--! @brief      Generates the next Round Key
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

entity KeyUpdate is
    port (
        clk : in std_logic;
        shift_in : in std_logic;
        sel_row : in std_logic_vector(1 downto 0);
        en_col_diff : in std_logic;
        key_input : in std_logic_vector(0 to CCSW - 1);
        key_output : out std_logic_vector(0 to CCSW - 1);
        col_diff_out : out std_logic_vector(0 to CCSW - 1);
        const_add_in : in std_logic_vector(0 to CCSW - 1);
        round_cnt : in std_logic_vector(0 to log2_ceil(NB_ROUNDS_KS_C) - 1)
    );
end entity KeyUpdate;

architecture rtl of KeyUpdate is
    signal in_sr : std_logic_vector(0 to NUM_MATRIX_ROWS*CCSW - 1);
    signal ki_r0 : std_logic_vector(0 to CCSW - 1);
    signal ki_r1 : std_logic_vector(0 to CCSW - 1);
    signal ki_r2 : std_logic_vector(0 to CCSW - 1);
    signal ki_r3 : std_logic_vector(0 to CCSW - 1);
    signal ki_xor : std_logic_vector(0 to CCSW - 1);
    signal ki_xor_r : std_logic_vector(0 to CCSW - 1);
    signal mix_col_s : std_logic_vector(0 to CCSW - 1);
    signal mix_rows_mux : std_logic_vector(0 to CCSW - 1);
    signal add_constant_mux : std_logic_vector(0 to CCSW - 1);
begin
    
    registers: process(clk)
    begin
        if rising_edge(clk) then
            if shift_in = '1' then
                in_sr <= in_sr(CCW to 4*CCW - 1) & key_input;
            end if;
            if en_col_diff = '1' then
                ki_xor_r <= ki_xor;
            end if;
        end if;
    end process registers;

    --Datapath
    
    ki_r0 <= in_sr(0 to CCW - 1);
    ki_r1 <= in_sr(CCW to 2*CCW - 1);
    ki_r2 <= in_sr(2*CCW to 3*CCW - 1);
    ki_r3 <= in_sr(3*CCW to 4*CCW - 1);

    ki_xor <= ki_r0 xor ki_r1 xor ki_r2 xor ki_r3;
    mix_col_s <= ki_r0 xor ki_xor_r; -- column diffusion step
    col_diff_out <= mix_col_s;

    with sel_row select mix_rows_mux <=
        const_add_in when "00",
        mix_col_s(CCSW - KS_ROT_GAP1 to CCSW - 1) & mix_col_s(0 to CCSW - KS_ROT_GAP1 - 1) when "01",
        mix_col_s(CCSW - KS_ROT_GAP2 to CCSW - 1) & mix_col_s(0 to CCSW - KS_ROT_GAP2 - 1) when "10",
        mix_col_s(CCSW - KS_ROT_GAP3 to CCSW - 1) & mix_col_s(0 to CCSW - KS_ROT_GAP3 - 1) when others;

    with sel_row select add_constant_mux <=
        KS_CONSTANT_0 & round_cnt when "00",
        KS_CONSTANT_1 when "01",
        KS_CONSTANT_2 when "10",
        KS_CONSTANT_3 when others;

    key_output <= mix_rows_mux xor add_constant_mux;
end architecture rtl;
