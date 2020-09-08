library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.design_pkg.all;

entity SubbytesRom is
    port (
        addr : in std_logic_vector(0 to 4);
        dout : out std_logic_vector(0 to 3)
    );
end entity SubbytesRom;

architecture rtl of SubbytesRom is
    type rom_type is array(0 to 2**5 - 1) of std_logic_vector(0 to 3);
        signal ROM : rom_type :=
    (
        -- sbox
        x"2", x"d", x"3", x"9", 
        x"7", x"b", x"a", x"6", 
        x"e", x"0", x"f", x"4", 
        x"8", x"5", x"1", x"c",

        -- inv sbox
        x"9", x"e", x"0", x"2",
        x"b", x"d", x"7", x"4",
        x"c", x"3", x"6", x"5",
        x"f", x"1", x"8", x"a"
    );
    attribute rom_style:string;
    attribute rom_style of ROM : signal is "distributed";

begin
    dout <= ROM(to_integer(unsigned(addr))); -- async read
end architecture rtl;