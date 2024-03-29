library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;

library mylib;
use mylib.defAT93C46DController.all;

package defPhaseEEPROM is
   
  constant kWordsCenter     : integer := 4;
  constant kWordsLength     : integer := 4;
  
  constant kAddrShiftCenter : integer := 0;
  constant kAddrShiftLength : integer := kAddrShiftCenter+kWordsCenter;
  constant kNumWords        : integer := kWordsCenter+kWordsLength;
  
  type DataArray    is array (integer range kNumWords-1 downto 0) of std_logic_vector(kWidthData-1 downto 0);
  
end package defPhaseEEPROM;
