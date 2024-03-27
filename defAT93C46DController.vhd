library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;

library mylib;

package defAT93C46DController is
  
  -- 125MHz/250kMHz=500
  constant kClockDivider  : integer := 500;
--  constant kClockDivider  : integer := 10;
  
  constant kWidthAddr     : integer := 7;
  constant kWidthData     : integer := 8;
  
  constant kMaxWidthDi    : integer := 18;
  constant kMaxWidthDo    : integer := kWidthData+1;
  
  constant kWidthDiEWEN   : integer := 12;
  constant kDiEWEN        : std_logic_vector(kWidthDiEWEN-1 downto 0) := "100110000000";
  constant kWidthDiEWDS   : integer := 12;
  constant kDiEWDS        : std_logic_vector(kWidthDiEWDS-1 downto 0) := "100000000000";
  
  constant kWidthOpcode   : integer := 3;
  constant kOpcodeREAD    : std_logic_vector(kWidthOpcode-1 downto 0) := "110";
  constant kOpcodeWRITE   : std_logic_vector(kWidthOpcode-1 downto 0) := "101";
  constant kOpcodeERASE   : std_logic_vector(kWidthOpcode-1 downto 0) := "111";
  
  type instructionType is (
    idle, EWEN, EWDS, READ, WRITE, ERASE
  );

  -- spi status
   -- EWEN/EWDS
    -- Idle -> dataOut -> waitTcs
   -- WRITE/ERASE
    -- Idle -> dataOut -> waitWETcs -> waitReady -> waitTcs
   -- READ
    -- Idle -> dataOut -> dataIn -> waitTcs
  type spiStatusType is (
    idle, tx, tr, waitTcs, waitWETcs, waitReady
  );
  
end package defAT93C46DController;
