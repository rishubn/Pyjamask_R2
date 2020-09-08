--------------------------------------------------------------------------------
--! @file       SubbytesRom.vhd
--! @brief      S-Box for Pyjamask block cipher
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
