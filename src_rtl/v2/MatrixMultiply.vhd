library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_misc.all;

-- Computes B = A*matrix
entity MatrixMultiply is
    generic (
        DataWidth : integer := 32;
        PipelineDepth : integer := 2
    );
    port (
        clk : in std_logic;
        din : in std_logic_vector(0 to DataWidth - 1);
        din_valid : in std_logic;
        matrix : in std_logic_vector(0 to DataWidth - 1);
        dout : out std_logic_vector(0 to DataWidth - 1);
        dout_valid : out std_logic
    );
end entity MatrixMultiply;

architecture rtl of MatrixMultiply is

    type add_array_t is array (0 to 15) of std_logic_vector(0 to DataWidth - 1);
    type matrix_st is array(0 to 15) of std_logic_vector(0 to DataWidth - 1);

    signal matrix_s, matrix_sr : matrix_st;
    signal matrix_r : std_logic_vector(0 to DataWidth - 1);
    signal xor_out, xor_out_r : add_array_t;-- , xor_out_r : std_logic_vector(0 to DataWidth - 1);
    signal din_r : std_logic_vector(0 to DataWidth - 1);
    signal xor_r : std_logic_vector(0 to DataWidth - 1);

  --  function xor_all(inputarr : matrix_st) return std_logic_vector(0 to DataWidth - 1) is
 --       variable output_s : std_logic_vector(0 to DataWidth - 1);
 --   begin
 --       for i in 0 to 15 loop

begin


   registers : process(clk)
   begin
    if rising_edge(clk) then
        if din_valid = '1' then
            matrix_r <= matrix(Datawidth - 16 to DataWidth - 1) & matrix(0 to DataWidth - 16 - 1);
            din_r <= din;
            xor_r <= xor_out(15);
        end if;
        dout_valid <= din_valid;
    end if;
  end process registers;


  matrix_rot_gen : for i in 0 to 15 generate
  --  first : if i < 16 generate
        matrix_s(i) <= matrix(Datawidth - i to DataWidth - 1) & matrix(0 to DataWidth - i - 1) when din(i) = '1' else (others => '0');
   -- end generate first;
  --  second : if i >= 16 generate
        matrix_sr(i) <= matrix_r(Datawidth - i to DataWidth - 1) & matrix_r(0 to DataWidth - i - 1) when din_r(i+16) = '1' else (others => '0');
    --end generate second;
  end generate matrix_rot_gen;


  xor_gen : for i in 0 to 15 generate
    first : if i = 0 generate
        xor_out(0) <= matrix_s(0);
        xor_out_r(0) <= xor_r xor matrix_sr(0);
    end generate first;
    second : if i > 0 generate
        xor_out(i) <= xor_out(i - 1) xor matrix_s(i);
        xor_out_r(i) <= xor_out_r(i - 1) xor matrix_sr(i);
    end generate second;
  end generate xor_gen;

  dout <= xor_out_r(15);


end architecture rtl;
