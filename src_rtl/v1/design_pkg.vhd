--------------------------------------------------------------------------------
--! @file       design_pkg.vhd
--! @brief      Design_pkg for CryptoCore
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
use IEEE.STD_LOGIC_1164.all;
use ieee.numeric_std.all;

package Design_pkg is

    --! design parameters needed by the PreProcessor, PostProcessor, and LWC; ssigned in the package body below!

    constant TAG_SIZE        : integer; --! Tag size
    constant HASH_VALUE_SIZE : integer; --! Hash value size

    constant CCSW            : integer; --! Internal key width. If SW = 8 or 16, CCSW = SW. If SW=32, CCSW = 8, 16, or 32.
    constant CCW             : integer; --! Internal data width. If W = 8 or 16, CCW = W. If W=32, CCW = 8, 16, or 32.
    constant CCWdiv8         : integer; --! derived from the parameters above, assigned in the package body below.

    --! design parameters specific to the CryptoCore; assigned in the package body below!
    --! place declarations of your types here

    --! place declarations of your constants here
    constant NPUB_SIZE       : integer;
    constant DBLK_SIZE       : integer;
    constant MSG_CNT_WIDTH : integer;
    
    constant COL_MK             : std_logic_vector;
    constant COL_M0             : std_logic_vector;
    constant COL_M1             : std_logic_vector;
    constant COL_M2             : std_logic_vector;
    constant COL_M3             : std_logic_vector;

    constant COL_INV_M0         : std_logic_vector;
    constant COL_INV_M1         : std_logic_vector;
    constant COL_INV_M2         : std_logic_vector;
    constant COL_INV_M3         : std_logic_vector;

    constant KS_ROT_GAP1        : integer;
    constant KS_ROT_GAP2        : integer;
    constant KS_ROT_GAP3        : integer;

    constant KS_CONSTANT_0      : std_logic_vector;
    constant KS_CONSTANT_1      : std_logic_vector;
    constant KS_CONSTANT_2      : std_logic_vector;
    constant KS_CONSTANT_3      : std_logic_vector;

    constant NB_ROUNDS_KS_C : integer;
    constant BLOCK_WORDS_C      : integer;
    constant KEY_ADDR_BITS_C    : integer;
    
    -- Generics
    constant MM_Depth : integer; -- Number of MatrixMultiply operations done in one clock cycle. Valid choices: 1, 2, 4, 8, 16, 32. Default: 16
    ----
    constant NUM_MATRIX_ROWS : integer;
    constant DBL_C : std_logic_vector;
    constant NONCE_C : std_logic_vector;
    constant OFFSET_ADDR_C : integer;
    constant AUTH_ADDR_C : integer;
    --! place declarations of your functions here
    function get_words(size: integer; iowidth:integer) return integer; 
    --! Calculate log2 and round up.
    function log2_ceil (N: natural) return natural;
    --! Padding the current word.
    function padd( bdi, bdi_valid_bytes, bdi_pad_loc : std_logic_vector ) return std_logic_vector;
    function ntz(n : integer) return integer; 
end Design_pkg;

package body Design_pkg is

  --! assign values to all constants and aliases here
    constant TAG_SIZE : integer := 128;
    constant HASH_VALUE_SIZE : integer := 128;
    constant CCSW : integer := 32;
    constant CCW : integer := 32;
    constant CCWdiv8 : integer := 4;
    constant NPUB_SIZE       : integer := 96;  --! Npub size
    constant DBLK_SIZE       : integer := 128; --! Block size
    constant MSG_CNT_WIDTH : integer := 16;
    constant COL_MK             : std_logic_vector := x"b881b9ca";
    constant COL_M0             : std_logic_vector := x"a3861085";
    constant COL_M1             : std_logic_vector := x"63417021";
    constant COL_M2             : std_logic_vector := x"692cf280";
    constant COL_M3             : std_logic_vector := x"48a54813";

    constant COL_INV_M0         : std_logic_vector := x"2037a121";
    constant COL_INV_M1         : std_logic_vector := x"108ff2a0";
    constant COL_INV_M2         : std_logic_vector := x"9054d8c0";
    constant COL_INV_M3         : std_logic_vector := x"3354b117";

    constant KS_ROT_GAP1        : integer := 8;
    constant KS_ROT_GAP2        : integer := 15;
    constant KS_ROT_GAP3        : integer := 18;

    constant KS_CONSTANT_0      : std_logic_vector := x"0000008";
    constant KS_CONSTANT_1      : std_logic_vector := x"00006a00";
    constant KS_CONSTANT_2      : std_logic_vector := x"003f0000";
    constant KS_CONSTANT_3      : std_logic_vector := x"24000000";

    constant NB_ROUNDS_KS_C : integer := 15; 

    --Generics
    constant MM_Depth : integer := 16;
    constant DBL_C : std_logic_vector := x"87";
    constant NONCE_C : std_logic_vector := x"00000001";
    constant AUTH_ADDR_C : integer := 0;
    constant OFFSET_ADDR_C : integer := 1;
  --! define your functions here
    --! Calculate the number of words
    function get_words(size: integer; iowidth:integer) return integer is
    begin
        if (size mod iowidth) > 0 then
            return size/iowidth + 1;
        else
            return size/iowidth;
        end if;
    end function get_words;

    function log2_ceil (N: natural) return natural is
    begin
        if ( N = 0 ) then
            return 0;
        elsif N <= 2 then
            return 1;
        else
            if (N mod 2 = 0) then
                return 1 + log2_ceil(N/2);
            else
                return 1 + log2_ceil((N+1)/2);
            end if;
        end if;
    end function log2_ceil;

    constant BLOCK_WORDS_C      : integer   := get_words(DBLK_SIZE, CCW);
    constant NUM_MATRIX_ROWS    : integer := get_words(DBLK_SIZE, CCSW);
    constant KEY_ADDR_BITS_C    : integer   := log2_ceil(NB_ROUNDS_KS_C) + log2_ceil(BLOCK_WORDS_C);
    --! Padd the data with 0x80 Byte if pad_loc is set.
    function padd( bdi, bdi_valid_bytes, bdi_pad_loc : std_logic_vector) return std_logic_vector is
        variable res : std_logic_vector(bdi'length - 1 downto 0) := (others => '0');
        variable inp : std_logic_vector(bdi'length - 1 downto 0);
    begin
        inp := bdi;
        for i in 0 to (bdi_valid_bytes'length - 1) loop
            if (bdi_valid_bytes(i) = '1') then
                res(8*(i+1) - 1 downto 8*i) := inp(8*(i+1) - 1 downto 8*i);
            elsif (bdi_pad_loc(i) = '1') then
                res(8*(i+1) - 1 downto 8*i) := x"80";
            else
                res(8*(i+1) - 1 downto 8*i) := x"00";
            end if;
        end loop;

        return res;
    end function;

    function ntz(n : integer) return integer is
        variable n_slv : std_logic_vector(MSG_CNT_WIDTH - 1 downto 0);
        variable count : integer range 0 to MSG_CNT_WIDTH;
    begin
        count := 0;
        n_slv := std_logic_vector(to_unsigned(n, MSG_CNT_WIDTH));
        for i in 0 to MSG_CNT_WIDTH - 1 loop
            exit when n_slv(i) = '1';
            count := count + 1;
        end loop;
        return count;
    end function;
end package body Design_pkg;
