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
    clk             : in  std_logic;  -- base clock
    rst             : in  std_logic;  -- base clock reset
    clk_sys         : in  std_logic;  -- system clock
    rst_sys         : in  std_logic;  -- system clock
    
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
  signal shift_value        : std_logic_vector(kWidthShift-1 downto 0);
  signal is_required_phase  : std_logic;
  signal rst_phase          : std_logic;
  signal rst_cdce           : std_logic;
  signal err_timeout        : std_logic;
  signal is_ready           : std_logic;
  signal status_function    : std_logic_vector(7 downto 0);
  
  -- reset
  signal init_cbt           : std_logic;
  
  -- eeprom
  signal eeprom_mean_we     : std_logic :='0';
  signal eeprom_mean_wd     : std_logic_vector(kWidthShift-1 downto 0);
  signal eeprom_mean_busy   : std_logic;
  signal eeprom_width_we    : std_logic :='0';
  signal eeprom_width_wd    : std_logic_vector(kWidthShift-1 downto 0);
  signal eeprom_width_busy  : std_logic;
  
  signal eeprom_mean_re     : std_logic :='0';
  signal eeprom_mean_rd     : std_logic_vector(kWidthShift-1 downto 0);
  signal eeprom_mean_valid  : std_logic;
  signal eeprom_width_re    : std_logic :='0';
  signal eeprom_width_rd    : std_logic_vector(kWidthShift-1 downto 0);
  signal eeprom_width_valid : std_logic;
  
  -- user local bus
  signal bus_rst_phase      : std_logic :='0';
  signal bus_mean_we        : std_logic :='0';
  signal bus_mean_wd        : std_logic_vector(kWidthShift-1 downto 0);
  signal bus_width_we       : std_logic :='0';
  signal bus_width_wd       : std_logic_vector(kWidthShift-1 downto 0);
  
  signal bus_status_function: std_logic_vector(7 downto 0);
  signal bus_shift_value    : std_logic_vector(kWidthShift-1 downto 0);
  signal bus_mean_rd        : std_logic_vector(kWidthShift-1 downto 0);
  signal bus_width_rd       : std_logic_vector(kWidthShift-1 downto 0);
  
  -- local bus --
  signal state_lbus           : BusProcessType;
  
  attribute mark_debug : boolean;
  attribute mark_debug of input_tapvalue    : signal is enDEBUG;
  attribute mark_debug of input_bitslipnum  : signal is enDEBUG;
  attribute mark_debug of shift_value       : signal is enDEBUG;
  attribute mark_debug of eeprom_mean_we    : signal is enDEBUG;
  attribute mark_debug of eeprom_mean_wd    : signal is enDEBUG;
  attribute mark_debug of eeprom_mean_busy  : signal is enDEBUG;
  attribute mark_debug of eeprom_mean_re    : signal is enDEBUG;
  attribute mark_debug of eeprom_mean_rd    : signal is enDEBUG;
  attribute mark_debug of eeprom_mean_valid : signal is enDEBUG;
  attribute mark_debug of eeprom_width_we   : signal is enDEBUG;
  attribute mark_debug of eeprom_width_wd   : signal is enDEBUG;
  attribute mark_debug of eeprom_width_busy : signal is enDEBUG;
  attribute mark_debug of eeprom_width_re   : signal is enDEBUG;
  attribute mark_debug of eeprom_width_rd   : signal is enDEBUG;
  attribute mark_debug of eeprom_width_valid: signal is enDEBUG;
begin

  rstCECE62002  <= output_rst_cdce;
  initCBT       <= output_init_cbt;
  isReady       <= output_is_ready;
  
  -- clock domain crossing ------------------------------------------------------------
  -- input signal
  xpm_cdc_mikumarilinkup : xpm_cdc_single
  port map (
    src_clk  => clk,
    dest_clk => clk_sys,
    src_in   => mikumariLinkUp,
    dest_out => input_mikumarilinkup
  );
  
  xpm_cdc_tapvalue : xpm_cdc_array_single
  generic map (
    WIDTH          => kWidthTap
  )
  port map (
    src_clk  => clk,
    dest_clk => clk_sys,
    src_in   => tapValue,
    dest_out => input_tapvalue
  );
  
  xpm_cdc_bitslipnum : xpm_cdc_array_single
  generic map (
    width => kWidthBitSlipNum
  )
  port map (
    src_clk  => clk,
    dest_clk => clk_sys,
    src_in   => bitSlipNum,
    dest_out => input_bitslipnum
  );
  
  syn_cdcelocked : entity mylib.synchronizer
  port map (
    clk   => clk_sys,
    dIn   => cdceLocked,
    dOut  => input_cdcelocked
  );
  
  -- output signal
  xpm_cdc_init_cbt : xpm_cdc_single
  port map (
    src_clk  => clk_sys,
    dest_clk => clk,
    src_in   => init_cbt,
    dest_out => output_init_cbt
  );
  
  xpm_cdc_is_ready : xpm_cdc_single
  port map (
    src_clk  => clk_sys,
    dest_clk => clk,
    src_in   => is_ready,
    dest_out => output_is_ready
  );

  -- user local bus
  xpm_cdc_rst_phase : xpm_cdc_pulse
  port map (
    src_clk     => clk,
    src_rst     => rst,
    dest_clk    => clk_sys,
    dest_rst    => rst_sys,
    src_pulse   => bus_rst_phase,
    dest_pulse  => rst_phase
  );
  
  xpm_cdc_mean_we : xpm_cdc_pulse
  port map (
    src_clk     => clk,
    src_rst     => rst,
    dest_clk    => clk_sys,
    dest_rst    => rst_sys,
    src_pulse   => bus_mean_we,
    dest_pulse  => eeprom_mean_we
  );

  xpm_cdc_mean_wd : xpm_cdc_array_single
  generic map (
    width => kWidthShift
  )
  port map (
    src_clk  => clk,
    dest_clk => clk_sys,
    src_in   => bus_mean_wd,
    dest_out => eeprom_mean_wd
  );
  
  xpm_cdc_width_we : xpm_cdc_pulse
  port map (
    src_clk     => clk,
    src_rst     => rst,
    dest_clk    => clk_sys,
    dest_rst    => rst_sys,
    src_pulse   => bus_width_we,
    dest_pulse  => eeprom_width_we
  );

  xpm_cdc_width_wd : xpm_cdc_array_single
  generic map (
    width => kWidthShift
  )
  port map (
    src_clk  => clk,
    dest_clk => clk_sys,
    src_in   => bus_width_wd,
    dest_out => eeprom_width_wd
  );
  
  xpm_cdc_status_function : xpm_cdc_array_single
  generic map (
    width => 8
  )
  port map (
    src_clk  => clk_sys,
    dest_clk => clk,
    src_in   => status_function,
    dest_out => bus_status_function
  );

  xpm_cdc_shift_value : xpm_cdc_array_single
  generic map (
    width => kWidthShift
  )
  port map (
    src_clk  => clk_sys,
    dest_clk => clk,
    src_in   => shift_value,
    dest_out => bus_shift_value
  );

  xpm_cdc_mean_rd : xpm_cdc_array_single
  generic map (
    width => kWidthShift
  )
  port map (
    src_clk  => clk_sys,
    dest_clk => clk,
    src_in   => eeprom_mean_rd,
    dest_out => bus_mean_rd
  );
  
  xpm_cdc_width_rd : xpm_cdc_array_single
  generic map (
    width => kWidthShift
  )
  port map (
    src_clk  => clk_sys,
    dest_clk => clk,
    src_in   => eeprom_width_rd,
    dest_out => bus_width_rd
  );

  -- system clock domain --------------------------------------------------------------
  -- phase_selection_process
  status_function(7 downto 2)     <= (others=>'0');
  status_function(kIndexTimeout)  <= err_timeout;
  status_function(kIndexIsReady)  <= is_ready;
  shift_value       <= std_logic_vector(EvaluateShift(input_tapvalue,input_bitslipnum)); 
  is_required_phase <= EvaluatePhase(input_tapvalue,input_bitslipnum,eeprom_mean_rd,eeprom_width_rd);
  
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
      if(rst_phase='1')then   -- reset CDCE62002 clock
        counter_timeout := kMaxTimeCdceReset;
        rst_cdce        <= '1';
        err_timeout     <= '0';
        is_ready        <= '1';
      elsif(mode='1')then     -- don't care clock phase
        rst_cdce        <= '0';
        err_timeout     <= '0';
        is_ready        <= '1';
      elsif(finish_init_cbt='0' or input_mikumarilinkup='0' or eeprom_mean_valid='0' or eeprom_width_valid='0')then -- wait identify phase
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
  
  -- eeprom_operation_process
  -- mean read
  u_mean_read_process : process(clk_sys,rst_sys)
    variable buf_mean_valid : std_logic := '0';
    variable wait_valid     : std_logic := '0';
  begin
    if(rst_sys = '1') then
      wait_valid      := '0';
      eeprom_mean_re  <= '0';
    elsif(clk_sys'event and clk_sys = '1') then
        if(eeprom_mean_we='1')then
          wait_valid      := '1';
          eeprom_mean_re  <= '1';
        elsif(wait_valid='0' and eeprom_mean_valid='0')then
          wait_valid      := '1';
          eeprom_mean_re  <= '1';
        else
          eeprom_mean_re  <= '0';
        end if;
        if(eeprom_mean_valid='1' and buf_mean_valid='0')then
          wait_valid      := '0';
        end if;
        buf_mean_valid  := eeprom_mean_valid;
    end if;
  end process u_mean_read_process;
  
  -- width read
  u_width_read_process : process(clk_sys,rst_sys)
    variable buf_width_valid  : std_logic := '0';
    variable wait_valid       : std_logic := '0';
  begin
    if(rst_sys = '1') then
      wait_valid      := '0';
      eeprom_width_re <= '0';
    elsif(clk_sys'event and clk_sys = '1') then
        if(eeprom_width_we='1')then
          wait_valid      := '1';
          eeprom_width_re <= '1';
        elsif(wait_valid='0' and eeprom_width_valid='0')then
          wait_valid      := '1';
          eeprom_width_re <= '1';
        else
          eeprom_width_re <= '0';
        end if;
        if(eeprom_width_valid='1' and buf_width_valid='0')then
          wait_valid      := '0';
        end if;
        buf_width_valid  := eeprom_width_valid;
    end if;
  end process u_width_read_process;
  
  -- eeprom
  u_phaseEEPROM : entity mylib.phaseEEPROM
  generic map(
    enDEBUG         => enDEBUG
  )
  port map(
    clk             => clk_sys,
    rst             => rst_sys,
    
    writeEnableMean => eeprom_mean_we, 
    writeDataMean   => eeprom_mean_wd,
    writeBusyMean   => eeprom_mean_busy,
    writeEnableWidth=> eeprom_width_we,
    writeDataWidth  => eeprom_width_wd,
    writeBusyWidth  => eeprom_width_busy,
    
    readEnableMean  => eeprom_mean_re,
    readDataMean    => eeprom_mean_rd,
    readValidMean   => eeprom_mean_valid,
    readEnableWidth => eeprom_width_re,
    readDataWidth   => eeprom_width_rd,
    readValidWidth  => eeprom_width_valid,
    
    CS              => CS,
    SK              => SK,
    DI              => DI,
    DO              => DO
  );
  
  -- operate_process
  -- rst_cdce
  u_rst_cdce_process : process(clk_sys)
    variable buf_rst_cdce : std_logic := '0';
    variable counter      : integer   := 0;
  begin
    if(clk_sys'event and clk_sys = '1') then
      if(rst_cdce='1' and buf_rst_cdce='0')then
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
      if(output_rst_cdce='1' or input_cdcelocked='0')then
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
  u_bus_process : process(clk, rst)
  begin
    if(rst = '1') then
      state_lbus          <= Init;
      dataLocalBusOut     <= (others=>'0');
      readyLocalBus		    <= '0';
      bus_mean_we         <= '0';
      bus_width_we        <= '0';
      bus_rst_phase       <= '0';
    
    elsif(clk'event and clk = '1') then
      case state_lbus is
        when Init =>
          state_lbus		      <= Idle;
          dataLocalBusOut     <= (others=>'0');
          readyLocalBus		    <= '0';
          bus_mean_we         <= '0';
          bus_width_we        <= '0';
          bus_rst_phase       <= '0';
          
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
          
          elsif(addrLocalBus(kNonMultiByte'range) = kAddrEepromMean(kNonMultiByte'range)) then
            case addrLocalBus(kMultiByte'range) is
              when k1stByte =>
                bus_mean_wd(7 downto 0) <= dataLocalBusIn;
              when k2ndByte =>
                bus_mean_wd(15 downto 8) <= dataLocalBusIn;
              when k3rdByte =>
                bus_mean_wd(23 downto 16) <= dataLocalBusIn;
              when k4thByte =>
                bus_mean_wd(31 downto 24) <= dataLocalBusIn;
                bus_mean_we <= '1';
              when others =>
                null;
            end case;
          
          elsif(addrLocalBus(kNonMultiByte'range) = kAddrEepromWidth(kNonMultiByte'range)) then
            case addrLocalBus(kMultiByte'range) is
              when k1stByte =>
                bus_width_wd(7 downto 0) <= dataLocalBusIn;
              when k2ndByte =>
                bus_width_wd(15 downto 8) <= dataLocalBusIn;
              when k3rdByte =>
                bus_width_wd(23 downto 16) <= dataLocalBusIn;
              when k4thByte =>
                bus_width_wd(31 downto 24) <= dataLocalBusIn;
                bus_width_we <= '1';
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
          
          elsif(addrLocalBus(kNonMultiByte'range) = kAddrEepromMean(kNonMultiByte'range)) then
            case addrLocalBus(kMultiByte'range) is
              when k1stByte =>
                dataLocalBusOut <= bus_mean_rd(7 downto 0);
              when k2ndByte =>
                dataLocalBusOut <= bus_mean_rd(15 downto 8);
              when k3rdByte =>
                dataLocalBusOut <= bus_mean_rd(23 downto 16);
              when k4thByte =>
                dataLocalBusOut <= bus_mean_rd(31 downto 24);
              when others =>
                null;
            end case;
          
          elsif(addrLocalBus(kNonMultiByte'range) = kAddrEepromWidth(kNonMultiByte'range)) then
            case addrLocalBus(kMultiByte'range) is
              when k1stByte =>
                dataLocalBusOut <= bus_width_rd(7 downto 0);
              when k2ndByte =>
                dataLocalBusOut <= bus_width_rd(15 downto 8);
              when k3rdByte =>
                dataLocalBusOut <= bus_width_rd(23 downto 16);
              when k4thByte =>
                dataLocalBusOut <= bus_width_rd(31 downto 24);
              when others =>
                null;
            end case;

          else
            null;
          end if;
          
          state_lbus	<= Done;
          
        when Done =>
          readyLocalBus	      <= '1';
          if(weLocalBus = '0' and reLocalBus = '0') then
            state_lbus	<= Idle;
          end if;
          
          bus_mean_we         <= '0';
          bus_width_we        <= '0';
          bus_rst_phase       <= '0';
          
        -- probably this is error --
        when others =>
          state_lbus	<= Init;
      end case;
    end if;
  end process u_bus_process;
  
end Behavioral;
