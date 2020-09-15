--------------------------------------------------------------------------------
--! @file       StretchHash.vhd
--! @brief      Generates the first offset for OCB mode
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
use work.design_pkg.all;

entity StretchHash is
    port (
        clk : in std_logic;
        ktop : in std_logic_vector(0 to CCW - 1);
        ld : in std_logic;
        rotate : in std_logic;
        rotate_o0 : in std_logic;
        offset : out std_logic_vector(0 to CCW - 1)
    );
end entity StretchHash;

architecture rtl of StretchHash is
    signal stretch : std_logic_vector(0 to 191);
    signal hash : std_logic_vector(0 to CCW - 1);
begin
    registers: process(clk)
    begin
        if rising_edge(clk) then
            if ld = '1' then
                stretch(0 to DBLK_SIZE -1) <= stretch(CCW to DBLK_SIZE - 1) & ktop;
                stretch(DBLK_SIZE to 191) <= stretch(DBLK_SIZE + CCW to 191) & hash;
            elsif rotate = '1' then
                stretch <= stretch(1 to 191) & stretch(0);
            elsif rotate_o0 = '1' then
                stretch <= stretch(CCW to DBLK_SIZE - 1) & stretch(0 to CCW - 1) & stretch(DBLK_SIZE to 191);
            end if;
            
        end if;
    end process registers;
    offset <= stretch(0 to CCW - 1);

    hash <= stretch(2*CCW to 3*CCW-1) xor stretch(8+2*CCW to 3*CCW+8-1);
end architecture rtl;
