library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;

library mylib;
use mylib.defBCT.all;
use mylib.defCDCM.all;

package defPhaseSelection is
   
  -- local bus
  -- clock staus
  constant kAddrPhaseStatus     : LocalAddressType  := x"000";  -- R,   [7:0]  get status of phase selection unit
    constant kIndexTimeout        : integer:= 1;
    constant kIndexIsReady        : integer:= 0;
  constant kAddrPhaseOperate    : LocalAddressType  := x"010";  -- W,   [7:0]  reset CDCE62002 clock phase
  constant kAddrShift           : LocalAddressType  := x"020";  -- R,   [32:0] get shift value
  
  -- eeprom
--  constant kAddrEepromStatus    : LocalAddressType  := x"100";  -- R,   [7:0]  get eeprom staus
--    constant kIndexEepromCenter   : integer:= 0;
--    constant kIndexEepromLength   : integer:= 1;
  constant kAddrEepromCenter    : LocalAddressType  := x"110";  -- W/R, [32:0] set/get eeprom acceptable_range_center value
  constant kAddrEepromLength    : LocalAddressType  := x"120";  -- W/R, [32:0] set/get eeprom acceptable_range_length value
  
  -- reset condition
  constant kWaitCdceReset   : integer := 16#05FFFFFF#;
  constant kWaitCdceStable  : integer := 16#05FFFFFF#;
  constant kMaxTimeCdceReset: integer := 16#0F#;
  
  -- shift value parameter
  constant kWidthShift      : integer := 32;
  constant kWidthTapSize    : integer := kWidthShift-kWidthTap;

  constant kTapSize         : unsigned(kWidthTapSize-1 downto 0) := "010011111110000000001100110";  -- 0.078 *(2^(32-5))
                                                                                                    -- = x"27F0066"

  function StepMap( bit_slip : std_logic_vector(kWidthBitSlipNum-1 downto 0)) return unsigned;
  function EvaluateShift( tap_value: std_logic_vector(kWidthTap-1 downto 0);
                          bit_slip : std_logic_vector(kWidthBitSlipNum-1 downto 0)
                          ) return unsigned;
  function EvaluatePhase( tap_value: std_logic_vector(kWidthTap-1 downto 0);
                          bit_slip : std_logic_vector(kWidthBitSlipNum-1 downto 0);
                          center   : std_logic_vector(kWidthShift-1 downto 0);
                          length   : std_logic_vector(kWidthShift-1 downto 0)      ) return std_logic;

end package defPhaseSelection;

package body defPhaseSelection is
  
  function StepMap( bit_slip : std_logic_vector(kWidthBitSlipNum-1 downto 0)
          ) return unsigned is                  
  begin
    case bit_slip is
      when X"0" => return x"00000000";  -- 0. *(2^(32-3))
      when X"1" => return x"20000000";  -- 1. *(2^(32-3))
      when X"2" => return x"C0000000";  -- 6. *(2^(32-3))
      when X"3" => return x"E0000000";  -- 7. *(2^(32-3))
      when X"4" => return x"80000000";  -- 4. *(2^(32-3))
      when X"5" => return x"A0000000";  -- 5. *(2^(32-3))
      when X"6" => return x"40000000";  -- 2. *(2^(32-3))
      when X"7" => return x"60000000";  -- 3. *(2^(32-3))
      when others => return x"00000000";
    end case;
  end StepMap;
  
  function EvaluateShift( tap_value: std_logic_vector(kWidthTap-1 downto 0);
                          bit_slip : std_logic_vector(kWidthBitSlipNum-1 downto 0)
                        ) return unsigned is
  begin
    return unsigned(tap_value)*kTapSize+StepMap(bit_slip);
  end EvaluateShift;
  
  function EvaluatePhase( tap_value: std_logic_vector(kWidthTap-1 downto 0);
                          bit_slip : std_logic_vector(kWidthBitSlipNum-1 downto 0);
                          center   : std_logic_vector(kWidthShift-1 downto 0);
                          length   : std_logic_vector(kWidthShift-1 downto 0)
                        ) return std_logic is
    variable is_right   : std_logic;
    variable length_by2 : std_logic_vector(kWidthShift-1 downto 0);
  begin
    length_by2(kWidthShift-1)          := '0';
    length_by2(kWidthShift-2 downto 0) := length(kWidthShift-1 downto 1);
    is_right := '1' when EvaluateShift(tap_value,bit_slip)+unsigned(length_by2)-unsigned(center)<unsigned(length) else '0';
    return is_right;
  end EvaluatePhase;

end package body defPhaseSelection;
