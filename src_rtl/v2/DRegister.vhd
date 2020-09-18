library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_misc.all;


-- Computes B = A*matrix
entity DRegister is
    generic (
        DataWidth : integer := 32
    );
    port (
        clk : in std_logic;
        D : in std_logic_vector(0 to DataWidth - 1);
        ld : in std_logic;
        Q : out std_logic_vector(0 to DataWidth - 1)
    );
end entity DRegister;

architecture rtl of DRegister is
    signal Q_s : std_logic_vector(0 to DataWidth - 1);
begin
    process(clk)
    begin
        if rising_edge(clk) then
            if ld = '1' then
                Q_s <= D;
            end if;
        end if;
    end process;
    Q <= Q_s;
end architecture rtl;