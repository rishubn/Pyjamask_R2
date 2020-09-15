--------------------------------------------------------------------------------
--! @file       MatrixMultiply.vhd
--! @brief      Multiplies a row of the state with a circulant matrix
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

-- Computes B = A*matrix
entity MatrixMultiply is
    generic (
        DataWidth : integer := 32;
        Depth : integer := 32
    );
    port (
        rst : in std_logic;
        clk : in std_logic;
        --data signals
        din : in std_logic_vector(0 to DataWidth - 1);
        matrix : in std_logic_vector(0 to DataWidth - 1);
        dout : out std_logic_vector(0 to DataWidth - 1);
        -- control signals
        start : in std_logic;
        done : out std_logic
        
    );
end entity MatrixMultiply;

architecture rtl of MatrixMultiply is

    signal matrix_r : std_logic_vector(0 to DataWidth - 1);
    signal matrix_mux : std_logic_vector(0 to DataWidth - 1);

    signal din_r : std_logic_vector(0 to DataWidth - 1);
    signal din_s : std_logic_vector(0 to Depth - 1);
    signal dout_r : std_logic_vector(0 to DataWidth - 1);
    signal dout_mux : std_logic_vector(0 to DataWidth - 1);
    signal sel_dout : std_logic;

    signal xor_mux : std_logic_vector(0 to DataWidth - 1);
    signal en_din : std_logic;
    signal shift_din : std_logic;

    signal en_dout : std_logic;
    signal en_matrix : std_logic;
    signal sel_matrix : std_logic;
    signal sel_xor : std_logic;
    signal din_cnt_r, din_cnt_s : integer range 0 to DataWidth/Depth - 1;

    type array_t is array (0 to Depth -1) of std_logic_vector(0 to DataWidth - 1);
    signal matrix_s : array_t;
    type multiply_array_t is array (0 to Depth - 1) of std_logic_vector(0 to DataWidth - 1);
    type add_array_t is array (0 to Depth - 2) of std_logic_vector(0 to DataWidth - 1);
    signal mux_out : array_t;
    signal xor_out : array_t;
    
    type state_t is (RESET, IDLE, MAT_MULTIPLY, OUTPUT);
    signal n_state_r, state_r : state_t;
begin
    

    FSM_state_register : process(clk)
    begin
        if rising_edge(clk) then
            if (rst = '1') then
                state_r <= RESET;
            else
                state_r <= n_state_r;
            end if;
        end if;
    end process FSM_state_register;

    FSM_control_path : process(all)
    begin
        en_din <= '0';
        en_dout <= '0';
        en_matrix <= '0';
        sel_matrix <= '0';
        sel_xor <= '0';
        shift_din <= '0';
        done <= '0';
        din_cnt_s <= din_cnt_r;
        n_state_r <= state_r;
        case state_r is
            when RESET =>
                din_cnt_s <= 0;
                n_state_r <= IDLE;
            when IDLE =>
                if start = '1' then
                    sel_xor <= '1';
                    en_dout <= '1'; -- clear dout register

                    en_matrix <= '1';
                    en_din <= '1';
                    sel_matrix <= '1';
                    n_state_r <= MAT_MULTIPLY;
                end if;
            when MAT_MULTIPLY =>
                en_dout <= '1';
                shift_din <= '1';
                en_matrix <= '1';
                if din_cnt_r = DataWidth/Depth - 1 then
                    n_state_r <= OUTPUT;
                else
                    din_cnt_s <= din_cnt_r + 1;
                end if;
            when OUTPUT =>
                done <= '1';
                n_state_r <= IDLE;
                din_cnt_s <= 0;
            end case;
    end process FSM_control_path;
-- datapath
    registers: process(clk)
    begin
        if rising_edge(clk) then
            if en_matrix = '1' then
                matrix_r <= matrix_mux;
            end if;
            if en_din = '1' then
                din_r <= din;
            elsif shift_din = '1' then
                din_r <= din_r(Depth to DataWidth - 1) & (0 to Depth - 1 => '0');
            end if;
            if en_dout = '1' then
                dout_r <= xor_out(Depth - 1);
            end if;
            din_cnt_r <= din_cnt_s;
        end if;
    end process registers;


    matrix_mux <= matrix when sel_matrix = '1' else matrix_s(Depth-1)(DataWidth - 1) & matrix_s(Depth-1)(0 to DataWidth - 2);

    matrix_s(0) <= matrix_r;
    mux_out(0) <= matrix_s(0) when din_s(0) = '1' else (others => '0');

    xor_mux <= (others => '0') when sel_xor = '1' else dout_r;
    xor_out(0) <= xor_mux xor mux_out(0);

    din_s <= din_r(0 to Depth - 1);
   -- dout_mux <= xor_out(Depth - 1) when sel_dout = '0' else (others => '0');

    matrix_rotate : for i in 1 to Depth - 1 generate
      matrix_s(i) <= matrix_r(Datawidth - i to Datawidth - 1) & matrix_r(0 to Datawidth - i - 1);
      mux_out(i) <= matrix_s(i) when din_s(i) = '1' else (others => '0');
      xor_out(i) <= xor_out(i-1) xor mux_out(i);
    end generate matrix_rotate;


--  matrix_rotate : for i in 1 to Depth - 1 generate
  --    matrix_s(i) <= matrix_s(i-1)(DataWidth - 1) & matrix_s(i-1)(0 to DataWidth - 2);
   --   mux_out(i) <= matrix_s(i) when din_r(i) = '1' else (others => '0');
    --  xor_out(i) <= xor_out(i - 1) xor mux_out(i);
    --d generate matrix_rotate;

    
    dout <= dout_r;

end architecture rtl;
