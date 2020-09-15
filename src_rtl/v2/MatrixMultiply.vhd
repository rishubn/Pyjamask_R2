library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_misc.all;

-- Computes B = A*matrix
entity MatrixMultiply is
    generic (
        DataWidth : integer := 32;
        PipelineDepth : integer := 4
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

    type ld_t is array(0 to PipelineDepth) of std_logic_vector(0 to 0);
    type register_t is array(0 to PipelineDepth) of std_logic_vector(0 to DataWidth - 1);
    type multiply_array_t is array (0 to DataWidth - 1) of std_logic_vector(0 to DataWidth - 1);
    type add_array_t is array (0 to DataWidth - 1) of std_logic_vector(0 to DataWidth - 1);
    type matrix_st is array(0 to DataWidth - 1) of std_logic_vector(0 to DataWidth - 1);
    
    signal matrix_s : matrix_st;
    signal matrix_r : register_t;
    signal mux_out : multiply_array_t;
    signal xor_out : add_array_t;
    signal ld_s : ld_t;
    signal din_r : register_t;
    signal out_r : register_t;
    component DRegister
        generic(
            DataWidth : integer
        );
        port(
            clk, ld : in std_logic;
            D : in std_logic_vector(0 to DataWidth - 1);
            Q : out std_logic_vector(0 to DataWidth - 1)
        );
    end component;
    
begin
    

   
    ld_s(0)(0) <= din_valid;
    din_r(0) <= din;
    xor_out(0) <= mux_out(0); --
    mux_out(0) <= matrix_r(0) when din_r(1)(0) = '1' else (others => '0');
    
    
    matrix_init_reg : DRegister generic map(DataWidth => DataWidth)
                port map(clk => clk, ld => ld_s(0)(0), D => matrix, Q => matrix_r(0));
    
    matrix_s(0) <= matrix_r(0)(DataWidth - 1) & matrix_r(0)(0 to DataWidth - 2);
    layer: for i in 0 to 3 generate
    
       -- pipeline_registers 
            ld_i_reg : DRegister generic map(DataWidth => 1)
                port map(clk => clk,D => ld_s(i), ld => '1', Q => ld_s(i+1));
                
            in_i_reg : DRegister generic map(DataWidth => DataWidth)
                            port map(clk => clk, ld => ld_s(i)(0), D => din_r(i), Q => din_r(i+1));
            
            xor_reg : if i > 0 generate
                mux_out(8*i) <= matrix_r(i) when din_r(i+1)(8*i) = '1' else (others => '0');
                xor_out(8*i) <= out_r(i - 1) xor mux_out(8*i);
                matrix_s(8*i) <= matrix_r(i)(DataWidth - 1) & matrix_r(i)(0 to DataWidth - 2);
                matrix_i_reg : DRegister generic map(DataWidth => DataWidth)
                    port map(clk => clk, ld => ld_s(i)(0), D => matrix_s(8*i - 1), Q => matrix_r(i));
                out_i_reg : DRegister generic map(DataWidth => DataWidth)
                    port map(clk => clk, ld => ld_s(i)(0), D => xor_out(8*i - 1), Q => out_r(i-1));
                
                
            end generate xor_reg;
            
            multiply : for j in 1 to 7 generate
                matrix_s(8*i+j) <= matrix_s(8*i+j - 1)(DataWidth - 1) & matrix_s(8*i+j - 1)(0 to DataWidth - 2);
                mux_out(8*i+j) <= matrix_s(8*i+j - 1) when din_r(i+1)(8*i+j) = '1' else (others => '0');
                xor_out(8*i+j) <= xor_out(8*i+j - 1) xor mux_out(8*i+j);
            end generate multiply;
            
        
   end generate layer;
    
    
    dout <= xor_out(31);
    dout_valid <= ld_s(PipelineDepth)(0);



end architecture rtl;
