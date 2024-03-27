library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;

library mylib;
use mylib.defAT93C46DController.all;

package defPhaseEEPROM is
   
  constant kAddrMean1   : std_logic_vector(kWidthAddr-1 downto 0) := "0000000";
  constant kAddrMean2   : std_logic_vector(kWidthAddr-1 downto 0) := "0000001";
  constant kAddrMean3   : std_logic_vector(kWidthAddr-1 downto 0) := "0000010";
  constant kAddrMean4   : std_logic_vector(kWidthAddr-1 downto 0) := "0000011";
  constant kAddrWidth1  : std_logic_vector(kWidthAddr-1 downto 0) := "0000100";
  constant kAddrWidth2  : std_logic_vector(kWidthAddr-1 downto 0) := "0000101";
  constant kAddrWidth3  : std_logic_vector(kWidthAddr-1 downto 0) := "0000110";
  constant kAddrWidth4  : std_logic_vector(kWidthAddr-1 downto 0) := "0000111";
  
  type operateType is (
    idle, writeMean, writeWidth, readMean, readWidth
  );
  
  type eepromStatusType is (
    idle, ewen, data1, data2, data3, data4, ewds, complete
  );
  
end package defPhaseEEPROM;
