library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;

library xpm;
use xpm.vcomponents.all;

library mylib;
use mylib.defBCT.all;
use mylib.defCDCM.all;
use mylib.defPhaseSelection.all;

entity phaseSelection is
  generic(
    -- DEBUG --
    enDEBUG         : boolean := false
  );
  port ( 
    clk_base        : in  std_logic;  -- base clock       (CDCE62002)
    rst_base        : in  std_logic;  -- base clock reset (CDCE62002)
    clk_sys         : in  std_logic;  -- system clock
    rst_sys         : in  std_logic;  -- system reset
    
    -- input information -------------------------------------------------------
    mode            : in  std_logic;  -- mode 0:non-require / 1:EEPROM
    mikumariLinkUp  : in  std_logic;
    tapValue        : in  std_logic_vector(kWidthTap-1 downto 0);
    bitSlipNum      : in  std_logic_vector(kWidthBitSlipNum-1 downto 0);
    cdceLocked      : in  std_logic;
    
    -- operate -----------------------------------------------------------------
    rstCECE62002    : out std_logic;
    initCBT         : out std_logic;
    
    -- status ------------------------------------------------------------------
    isReady         : out std_logic;
    
    -- EEPROM(AT93C460) --------------------------------------------------------
    CS              : out std_logic;  -- Chip Select
    SK              : out std_logic;  -- Serial Data Clock
    DI              : out std_logic;  -- Serial Data Input  (master-->slave)
    DO              : in  std_logic;  -- Serial Data Output (slave-->master)
    
    -- Local bus ---------------------------------------------------------------
    addrLocalBus    : in  LocalAddressType;
    dataLocalBusIn  : in  LocalBusInType;
    dataLocalBusOut	: out LocalBusOutType;
    reLocalBus      : in  std_logic;
    weLocalBus      : in  std_logic;
    readyLocalBus	  : out std_logic
  );
end phaseSelection;

architecture Behavioral of phaseSelection is
  -- input
  signal input_mikumarilinkup : std_logic;
  signal input_tapvalue       : std_logic_vector(kWidthTap-1 downto 0);
  signal input_bitslipnum     : std_logic_vector(kWidthBitSlipNum-1 downto 0);
  signal input_cdcelocked     : std_logic;
  
  -- output
  signal output_rst_cdce      : std_logic;
  signal output_init_cbt      : std_logic;
  signal output_is_ready      : std_logic;
 
  -- select phase
  signal shift_value          : std_logic_vector(kWidthShift-1 downto 0);
  signal is_required_phase    : std_logic;
  signal rst_phase            : std_logic;
  signal rst_cdce             : std_logic;
  signal err_timeout          : std_logic;
  signal is_ready             : std_logic;
  signal status_function      : std_logic_vector(7 downto 0);
  
  -- reset
  signal init_cbt             : std_logic;
  
  -- eeprom
  signal eeprom_center_we     : std_logic :='0';
  signal eeprom_center_wd     : std_logic_vector(kWidthShift-1 downto 0);
  signal eeprom_center_busy   : std_logic;
  signal eeprom_length_we     : std_logic :='0';
  signal eeprom_length_wd     : std_logic_vector(kWidthShift-1 downto 0);
  signal eeprom_length_busy   : std_logic;
  
  signal eeprom_center_rd     : std_logic_vector(kWidthShift-1 downto 0);
  signal eeprom_center_valid  : std_logic;
  signal eeprom_length_rd     : std_logic_vector(kWidthShift-1 downto 0);
  signal eeprom_length_valid  : std_logic;
  
  -- user local bus
  signal bus_rst_phase        : std_logic :='0';
  signal bus_center_we        : std_logic :='0';
  signal bus_center_wd        : std_logic_vector(kWidthShift-1 downto 0);
  signal bus_length_we        : std_logic :='0';
  signal bus_length_wd        : std_logic_vector(kWidthShift-1 downto 0);
  
  signal bus_status_function  : std_logic_vector(7 downto 0);
  signal bus_shift_value      : std_logic_vector(kWidthShift-1 downto 0);
  signal bus_center_rd        : std_logic_vector(kWidthShift-1 downto 0);
  signal bus_length_rd        : std_logic_vector(kWidthShift-1 downto 0);
  
  -- local bus --
  signal state_lbus           : BusProcessType;
  
  attribute mark_debug : boolean;
  attribute mark_debug of input_tapvalue      : signal is enDEBUG;
  attribute mark_debug of input_bitslipnum    : signal is enDEBUG;
  attribute mark_debug of shift_value         : signal is enDEBUG;
  attribute mark_debug of eeprom_center_we    : signal is enDEBUG;
  attribute mark_debug of eeprom_center_wd    : signal is enDEBUG;
  attribute mark_debug of eeprom_center_busy  : signal is enDEBUG;
  attribute mark_debug of eeprom_center_rd    : signal is enDEBUG;
  attribute mark_debug of eeprom_center_valid : signal is enDEBUG;
  attribute mark_debug of eeprom_length_we    : signal is enDEBUG;
  attribute mark_debug of eeprom_length_wd    : signal is enDEBUG;
  attribute mark_debug of eeprom_length_busy  : signal is enDEBUG;
  attribute mark_debug of eeprom_length_rd    : signal is enDEBUG;
  attribute mark_debug of eeprom_length_valid : signal is enDEBUG;
begin

  rstCECE62002  <= output_rst_cdce;
  initCBT       <= output_init_cbt;
  isReady       <= output_is_ready;
  
  -- clock domain crossing ------------------------------------------------------------
  -- input signal
  xpm_cdc_mikumarilinkup : xpm_cdc_single
  port map (
    src_clk   => clk_base,
    dest_clk  => clk_sys,
    src_in    => mikumariLinkUp,
    dest_out  => input_mikumarilinkup
  );
  
  xpm_cdc_tapvalue : xpm_cdc_array_single
  generic map (
    WIDTH     => kWidthTap
  )
  port map (
    src_clk   => clk_base,
    dest_clk  => clk_sys,
    src_in    => tapValue,
    dest_out  => input_tapvalue
  );
  
  xpm_cdc_bitslipnum : xpm_cdc_array_single
  generic map (
    width     => kWidthBitSlipNum
  )
  port map (
    src_clk   => clk_base,
    dest_clk  => clk_sys,
    src_in    => bitSlipNum,
    dest_out  => input_bitslipnum
  );
  
  syn_cdcelocked : entity mylib.synchronizer
  port map (
    clk       => clk_sys,
    dIn       => cdceLocked,
    dOut      => input_cdcelocked
  );
  
  -- output signal
  xpm_cdc_init_cbt : xpm_cdc_single
  port map (
    src_clk   => clk_sys,
    dest_clk  => clk_base,
    src_in    => init_cbt,
    dest_out  => output_init_cbt
  );
  
  xpm_cdc_is_ready : xpm_cdc_single
  port map (
    src_clk   => clk_sys,
    dest_clk  => clk_base,
    src_in    => is_ready,
    dest_out  => output_is_ready
  );

  -- user local bus
  xpm_cdc_rst_phase : xpm_cdc_pulse
  port map (
    src_clk     => clk_base,
    src_rst     => rst_base,
    dest_clk    => clk_sys,
    dest_rst    => rst_sys,
    src_pulse   => bus_rst_phase,
    dest_pulse  => rst_phase
  );
  
  xpm_cdc_center_we : xpm_cdc_pulse
  port map (
    src_clk     => clk_base,
    src_rst     => rst_base,
    dest_clk    => clk_sys,
    dest_rst    => rst_sys,
    src_pulse   => bus_center_we,
    dest_pulse  => eeprom_center_we
  );

  xpm_cdc_center_wd : xpm_cdc_array_single
  generic map (
    width     => kWidthShift
  )
  port map (
    src_clk   => clk_base,
    dest_clk  => clk_sys,
    src_in    => bus_center_wd,
    dest_out  => eeprom_center_wd
  );
  
  xpm_cdc_length_we : xpm_cdc_pulse
  port map (
    src_clk     => clk_base,
    src_rst     => rst_base,
    dest_clk    => clk_sys,
    dest_rst    => rst_sys,
    src_pulse   => bus_length_we,
    dest_pulse  => eeprom_length_we
  );

  xpm_cdc_length_wd : xpm_cdc_array_single
  generic map (
    width     => kWidthShift
  )
  port map (
    src_clk   => clk_base,
    dest_clk  => clk_sys,
    src_in    => bus_length_wd,
    dest_out  => eeprom_length_wd
  );
  
  xpm_cdc_status_function : xpm_cdc_array_single
  generic map (
    width     => 8
  )
  port map (
    src_clk   => clk_sys,
    dest_clk  => clk_base,
    src_in    => status_function,
    dest_out  => bus_status_function
  );

  xpm_cdc_shift_value : xpm_cdc_array_single
  generic map (
    width     => kWidthShift
  )
  port map (
    src_clk   => clk_sys,
    dest_clk  => clk_base,
    src_in    => shift_value,
    dest_out  => bus_shift_value
  );

  xpm_cdc_center_rd : xpm_cdc_array_single
  generic map (
    width     => kWidthShift
  )
  port map (
    src_clk   => clk_sys,
    dest_clk  => clk_base,
    src_in    => eeprom_center_rd,
    dest_out  => bus_center_rd
  );
  
  xpm_cdc_length_rd : xpm_cdc_array_single
  generic map (
    width     => kWidthShift
  )
  port map (
    src_clk   => clk_sys,
    dest_clk  => clk_base,
    src_in    => eeprom_length_rd,
    dest_out  => bus_length_rd
  );

  -- system clock domain --------------------------------------------------------------
  -- phase_selection_process
  status_function(7 downto 2)     <= (others=>'0');
  status_function(kIndexTimeout)  <= err_timeout;
  status_function(kIndexIsReady)  <= is_ready;
  shift_value       <= std_logic_vector(EvaluateShift(input_tapvalue,input_bitslipnum)); 
  is_required_phase <= EvaluatePhase(input_tapvalue,input_bitslipnum,eeprom_center_rd,eeprom_length_rd);
  
  u_rst_phase_selection_process : process(clk_sys,rst_sys)
    variable buf_init_cbt     : std_logic := '0';
    variable finish_init_cbt  : std_logic := '0';
    variable counter_timeout  : integer   := kMaxTimeCdceReset;
  begin
    if(rst_sys = '1') then
      buf_init_cbt    := '0';
      finish_init_cbt := '1';
      counter_timeout := kMaxTimeCdceReset;
      rst_cdce        <= '0';
      is_ready        <= '0';
      err_timeout     <= '0';
    elsif(clk_sys'event and clk_sys = '1') then
      -- When the CDCE62002 is reset?
      if(rst_phase='1')then   -- PC requires reset CDCE62002 clock
        counter_timeout := kMaxTimeCdceReset;
        rst_cdce        <= '1';
        err_timeout     <= '0';
        is_ready        <= '1';
      elsif(mode='1')then     -- don't care clock phase
        rst_cdce        <= '0';
        err_timeout     <= '0';
        is_ready        <= '1';
      elsif(finish_init_cbt='0' or input_mikumarilinkup='0' or eeprom_center_valid='0' or eeprom_length_valid='0')then -- wait identify phase
        rst_cdce        <= '0';
        is_ready        <= '0';
      elsif(is_required_phase='1')then  -- get required phase
        counter_timeout := 0;
        rst_cdce        <= '0';
        is_ready        <= '1';
      elsif(counter_timeout=0)then      -- timeout
        rst_cdce        <= '0';
        err_timeout     <= '1';
        is_ready        <= '0';
      else                              -- search the next phase
        finish_init_cbt := '0';
        counter_timeout := counter_timeout-1;
        rst_cdce        <= '1';
        is_ready        <= '0';
      end if;
      
      if(init_cbt='0' and buf_init_cbt='1')then
        finish_init_cbt  := '1';
      elsif(input_cdcelocked='0')then
        finish_init_cbt  := '0';
      end if;
      buf_init_cbt := init_cbt;
    end if;
  end process u_rst_phase_selection_process;
  
  -- eeprom
  u_phaseEEPROM : entity mylib.phaseEEPROM
  generic map(
    enDEBUG           => enDEBUG
  )
  port map(
    clk               => clk_sys,
    rst               => rst_sys,
    
    writeEnableCenter => eeprom_center_we, 
    writeDataCenter   => eeprom_center_wd,
    writeBusyCenter   => eeprom_center_busy,
    writeEnableLength => eeprom_length_we,
    writeDataLength   => eeprom_length_wd,
    writeBusyLength   => eeprom_length_busy,
    
    readDataCenter    => eeprom_center_rd,
    readValidCenter   => eeprom_center_valid,
    readDataLength    => eeprom_length_rd,
    readValidLength   => eeprom_length_valid,
    
    CS                => CS,
    SK                => SK,
    DI                => DI,
    DO                => DO
  );
  
  -- operate_process
  -- rst_cdce
  u_rst_cdce_process : process(clk_sys)
    variable buf_rst_cdce : std_logic := '0';
    variable counter      : integer   := 0;
  begin
    if(clk_sys'event and clk_sys = '1') then
      if(rst_cdce='1' and buf_rst_cdce='0')then -- this unit requires cdce reset
        counter         := kWaitCdceReset;
        output_rst_cdce <= '1';
      elsif(counter=0)then
        output_rst_cdce <= '0';
      else
        counter         := counter -1;
        output_rst_cdce <= '1';
      end if;
      buf_rst_cdce  := rst_cdce;
    end if;
  end process u_rst_cdce_process;

  -- init_cbt
  u_init_cbt_process : process(clk_sys)
    variable counter      : integer   := 0;
  begin
    if(clk_sys'event and clk_sys = '1') then
      if(output_rst_cdce='1' or input_cdcelocked='0')then -- this unit requires cdce reset / cdce is un-locked
        counter   := kWaitCdceStable;
        init_cbt  <= '1';
      elsif(counter=0)then
        init_cbt  <= '0';
      else
        counter   := counter -1;
        init_cbt  <= '1';
      end if;
    end if;
  end process u_init_cbt_process;
  
  -- based clock domain --------------------------------------------------------------
  -- local bus
  u_bus_process : process(clk_base, rst_base)
  begin
    if(rst_base = '1') then
      state_lbus      <= Init;
      dataLocalBusOut <= (others=>'0');
      readyLocalBus		<= '0';
      bus_center_we   <= '0';
      bus_length_we   <= '0';
      bus_rst_phase   <= '0';
    
    elsif(clk_base'event and clk_base = '1') then
      case state_lbus is
        when Init =>
          state_lbus		  <= Idle;
          dataLocalBusOut <= (others=>'0');
          readyLocalBus		<= '0';
          bus_center_we   <= '0';
          bus_length_we   <= '0';
          bus_rst_phase   <= '0';
          
        when Idle =>
          readyLocalBus	<= '0';
          if(weLocalBus = '1' or reLocalBus = '1') then
            state_lbus	<= Connect;
          end if;
          
        when Connect =>
          if(weLocalBus = '1') then
            state_lbus	<= Write;
          else
            state_lbus	<= Read;
          end if;
          
        when Write =>
          if(addrLocalBus(kNonMultiByte'range) = kAddrPhaseOperate(kNonMultiByte'range)) then
            bus_rst_phase <= '1';
          
          elsif(addrLocalBus(kNonMultiByte'range) = kAddrEepromCenter(kNonMultiByte'range)) then
            case addrLocalBus(kMultiByte'range) is
              when k1stByte =>
                bus_center_wd(7 downto 0)   <= dataLocalBusIn;
              when k2ndByte =>
                bus_center_wd(15 downto 8)  <= dataLocalBusIn;
              when k3rdByte =>
                bus_center_wd(23 downto 16) <= dataLocalBusIn;
              when k4thByte =>
                bus_center_wd(31 downto 24) <= dataLocalBusIn;
                bus_center_we <= '1';
              when others =>
                null;
            end case;
          
          elsif(addrLocalBus(kNonMultiByte'range) = kAddrEepromLength(kNonMultiByte'range)) then
            case addrLocalBus(kMultiByte'range) is
              when k1stByte =>
                bus_length_wd(7 downto 0)   <= dataLocalBusIn;
              when k2ndByte =>
                bus_length_wd(15 downto 8)  <= dataLocalBusIn;
              when k3rdByte =>
                bus_length_wd(23 downto 16) <= dataLocalBusIn;
              when k4thByte =>
                bus_length_wd(31 downto 24) <= dataLocalBusIn;
                bus_length_we <= '1';
              when others =>
                null;
            end case;
          
          else
            null;
          end if;

          state_lbus	<= Done;
          
        when Read =>
          if(addrLocalBus(kNonMultiByte'range) = kAddrPhaseStatus(kNonMultiByte'range)) then
                dataLocalBusOut <= bus_status_function;
          
          elsif(addrLocalBus(kNonMultiByte'range) = kAddrShift(kNonMultiByte'range)) then
            case addrLocalBus(kMultiByte'range) is
              when k1stByte =>
                dataLocalBusOut <= bus_shift_value(7 downto 0);
              when k2ndByte =>
                dataLocalBusOut <= bus_shift_value(15 downto 8);
              when k3rdByte =>
                dataLocalBusOut <= bus_shift_value(23 downto 16);
              when k4thByte =>
                dataLocalBusOut <= bus_shift_value(31 downto 24);
              when others =>
                null;
            end case;
          
          elsif(addrLocalBus(kNonMultiByte'range) = kAddrEepromCenter(kNonMultiByte'range)) then
            case addrLocalBus(kMultiByte'range) is
              when k1stByte =>
                dataLocalBusOut <= bus_center_rd(7 downto 0);
              when k2ndByte =>
                dataLocalBusOut <= bus_center_rd(15 downto 8);
              when k3rdByte =>
                dataLocalBusOut <= bus_center_rd(23 downto 16);
              when k4thByte =>
                dataLocalBusOut <= bus_center_rd(31 downto 24);
              when others =>
                null;
            end case;
          
          elsif(addrLocalBus(kNonMultiByte'range) = kAddrEepromLength(kNonMultiByte'range)) then
            case addrLocalBus(kMultiByte'range) is
              when k1stByte =>
                dataLocalBusOut <= bus_length_rd(7 downto 0);
              when k2ndByte =>
                dataLocalBusOut <= bus_length_rd(15 downto 8);
              when k3rdByte =>
                dataLocalBusOut <= bus_length_rd(23 downto 16);
              when k4thByte =>
                dataLocalBusOut <= bus_length_rd(31 downto 24);
              when others =>
                null;
            end case;

          else
            null;
          end if;
          
          state_lbus  <= Done;
          
        when Done =>
          readyLocalBus <= '1';
          if(weLocalBus = '0' and reLocalBus = '0') then
            state_lbus  <= Idle;
          end if;
          
          bus_center_we <= '0';
          bus_length_we <= '0';
          bus_rst_phase <= '0';
          
        -- probably this is error --
        when others =>
          state_lbus	<= Init;
      end case;
    end if;
  end process u_bus_process;
  
end Behavioral;
