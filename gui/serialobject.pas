unit serialobject;

interface

uses
  serial, sysutils;

const
  NCCS = 19;

type
  {$PACKRECORDS C}
    Termios2 = record
      c_iflag,
      c_oflag,
      c_cflag,
      c_lflag  : cardinal;
      c_line   : char;
      c_cc     : array[0..NCCS-1] of byte;
      c_ispeed,
      c_ospeed : cardinal;
    end;

  { TSerialObj }

  TSerialObj = class
  private
    FBaudRate: integer;
    {$IFNDEF WINDOWS}Ftios: Termios2;{$ENDIF}
    FHandle: THandle;
    procedure FSetParams(ByteSize: Integer; Parity: TParityType; StopBits: Integer;
      Flags: TSerialFlags);
    procedure FUpdateBaudRate(aBaud: integer);
  public
    portName: string;  // full name, e.g. /dev/ttyUSB0 or COM1
    function OpenPort(aPortName: string; baud: integer): boolean;
    function Read(var Buffer; Count: LongInt): integer;
    function ReadTimeout(var Buffer: array of byte; count, mSec: LongInt): integer;
    function ReadByteTimeout(var b: byte; mSec: LongInt): integer;
    function Write(var Buffer; Count: LongInt): LongInt;
    function Write(const b: byte): LongInt;

    procedure Flush;
    procedure FlushInput;
    procedure FlushOutput;

    constructor Create;
    destructor Destroy; override;

    property BaudRate: integer read FBaudRate write FUpdateBaudRate;
    property SerialHandle: THandle read FHandle;
  end;


// Return list of serial ports as a comma separated list
function GetSerialPortNames: string;

const
  TCGETS2 = $802C542A; //longint(2150388778);
  TCSETS2 = $402C542B; //1076646955;

implementation

uses
  {$IFDEF UNIX}
  termio, BaseUnix, errors;
  {$ELSE}
  windows, registry, classes;
  {$ENDIF}

{ Translated from include/linux/serial.h }
type
  TSerialStruct = packed record
    typ: Integer;
    line: Integer;
    port: Cardinal;
    irq: Integer;
    flags: Integer;
    xmit_fifo_size: Integer;
    custom_divisor: Integer;
    baud_base: Integer;
    close_delay: Word;
    io_type: Char;
    reserved_char: Char;
    hub6: Integer;
    closing_wait: Word; // time to wait before closing
    closing_wait2: Word; // no longer used...
    iomem_base: ^Char;
    iomem_reg_shift: Word;
    port_high: Cardinal;
    iomap_base: LongWord; // cookie passed into ioremap
  end;

{ TSerialObj }

{$ifdef windows}
// Need Windows equivalent still
// Check if custom baud is supported on Windows by checking COMMPROP.dwMaxBaud (https://msdn.microsoft.com/en-us/library/windows/desktop/aa363189(v=vs.85).aspx)
// Set custom baud directly in DCB.BaudRate (https://msdn.microsoft.com/en-us/library/windows/desktop/aa363214(v=vs.85).aspx)
procedure TSerialObj.FSetParams(ByteSize: Integer; Parity: TParityType;
  StopBits: Integer; Flags: TSerialFlags);
begin
  SerSetParams(FHandle, BaudRate, ByteSize, Parity, StopBits, Flags);
end;

{$else}
// For custom baud rate on Linux see e.g. 
// https://www.downtowndougbrown.com/2013/11/linux-custom-serial-baud-rates/
procedure TSerialObj.FSetParams(ByteSize: Integer; Parity: TParityType;
  StopBits: Integer; Flags: TSerialFlags);
var
  customBAUD: boolean;
begin
  FillChar(Ftios, SizeOf(Ftios), #0);
  customBAUD := false;

  case baudRate of
    50: Ftios.c_cflag := B50;
    75: Ftios.c_cflag := B75;
    110: Ftios.c_cflag := B110;
    134: Ftios.c_cflag := B134;
    150: Ftios.c_cflag := B150;
    200: Ftios.c_cflag := B200;
    300: Ftios.c_cflag := B300;
    600: Ftios.c_cflag := B600;
    1200: Ftios.c_cflag := B1200;
    1800: Ftios.c_cflag := B1800;
    2400: Ftios.c_cflag := B2400;
    4800: Ftios.c_cflag := B4800;
    19200: Ftios.c_cflag := B19200;
    38400: Ftios.c_cflag := B38400;
    57600: Ftios.c_cflag := B57600;
    115200: Ftios.c_cflag := B115200;
    230400: Ftios.c_cflag := B230400;
{$ifndef BSD}
    460800: Ftios.c_cflag := B460800;
{$endif}
    else
    begin
      customBAUD := true;
      Ftios.c_cflag     := CBAUDEX or CLOCAL;
      Ftios.c_iflag     := IGNPAR or IGNBRK;
      Ftios.c_oflag     := 0;
      Ftios.c_lflag     := 0;
      Ftios.c_ispeed    := baudRate;
      Ftios.c_ospeed    := baudRate;
      Ftios.c_cc[VMIN]  := 0;           // Return as soon as one byte is available
      Ftios.c_cc[VTIME] := 10;//5;           // 0.5 seconds timeout per byte
    end;
  end;

  Ftios.c_cflag := Ftios.c_cflag or CREAD or CLOCAL;

  case ByteSize of
    5: Ftios.c_cflag := Ftios.c_cflag or CS5;
    6: Ftios.c_cflag := Ftios.c_cflag or CS6;
    7: Ftios.c_cflag := Ftios.c_cflag or CS7;
    else Ftios.c_cflag := Ftios.c_cflag or CS8;
  end;

  case Parity of
    OddParity: Ftios.c_cflag := Ftios.c_cflag or PARENB or PARODD;
    EvenParity: Ftios.c_cflag := Ftios.c_cflag or PARENB;
  end;

  if StopBits = 2 then
    Ftios.c_cflag := Ftios.c_cflag or CSTOPB;

  if RtsCtsFlowControl in Flags then
    Ftios.c_cflag := Ftios.c_cflag or CRTSCTS;

  tcflush(FHandle, TCIOFLUSH);
  if customBAUD then
    FpIOCtl(FHandle, TCSETS2, @Ftios)
  else
    tcsetattr(FHandle, TCSANOW, Termios(pointer(@Ftios)^));
end;
{$endif}

procedure TSerialObj.FUpdateBaudRate(aBaud: integer);
begin
  FBaudRate := aBaud;
  self.FSetParams(8, NoneParity, 1, []);
end;

function TSerialObj.OpenPort(aPortName: string; baud: integer): boolean;
begin
  portName := aPortName;
  {$IFNDEF WINDOWS}
  FHandle := FpOpen(portName, O_RDWR or O_NOCTTY or O_NONBLOCK);
  {$ELSE}
  FHandle := SerOpen('\\.\' + aPortName);
  {$ENDIF}

  if {$IFNDEF WINDOWS}(FHandle < 0) {$ELSE} (FHandle = ERROR_INVALID_HANDLE) {$ENDIF} then
  begin
    WriteLn('Couldn''t open serial port: ', portname);
    result := false;
    portName := '';
  end
  else
  begin
    BaudRate := baud;
    Result := true;
  end;
end;

function TSerialObj.Read(var Buffer; Count: LongInt): integer;
begin
  result := SerRead(FHandle, Buffer, Count);
end;

function TSerialObj.ReadTimeout(var Buffer: array of byte; count, mSec: LongInt): integer;
begin
  result := SerReadTimeout(FHandle, Buffer[0], count, mSec);
end;

function TSerialObj.ReadByteTimeout(var b: byte; mSec: LongInt): integer;
begin
  result := SerReadTimeout(FHandle, b, 1, mSec);
end;

function TSerialObj.Write(var Buffer; Count: LongInt): LongInt;
begin
  result := SerWrite(FHandle, Buffer, Count);
end;

function TSerialObj.Write(const b: byte): LongInt;
var
  a: byte;
begin
  a := b;
  result := SerWrite(FHandle, a, 1);
end;

procedure TSerialObj.Flush;
begin
  SerFlush(FHandle);
end;

procedure TSerialObj.FlushInput;
begin
  SerFlushInput(FHandle);
end;

procedure TSerialObj.FlushOutput;
begin
  SerFlushOutput(FHandle);
end;

constructor TSerialObj.Create;
begin
  inherited Create;

  {$IFNDEF WINDOWS}FHandle := -1{$ELSE}FHandle := ERROR_INVALID_HANDLE{$ENDIF};
end;

destructor TSerialObj.Destroy;
begin
  if {$IFNDEF WINDOWS}FHandle > -1{$ELSE}FHandle > ERROR_INVALID_HANDLE{$ENDIF} then
  begin
    SerClose(FHandle);
    FHandle := 0;
  end;

  inherited Destroy;
end;

////////////////////////////////////////////////////////////////////////
  {$IFDEF MSWINDOWS}
  function GetSerialPortNames: string;
  var
    reg: TRegistry;
    l, v: TStringList;
    n: integer;
  begin
    l := TStringList.Create;
    v := TStringList.Create;
    reg := TRegistry.Create;
    try
  {$IFNDEF VER100}
  {$IFNDEF VER120}
      reg.Access := KEY_READ;
  {$ENDIF}
  {$ENDIF}
      reg.RootKey := HKEY_LOCAL_MACHINE;
      reg.OpenKey('\HARDWARE\DEVICEMAP\SERIALCOMM', false);
      reg.GetValueNames(l);
      for n := 0 to l.Count - 1 do
        v.Add(reg.ReadString(l[n]));
      Result := v.CommaText;
    finally
      reg.Free;
      l.Free;
      v.Free;
    end;
  end;
  {$ENDIF}
  {$IFNDEF MSWINDOWS}
  // Idea adapted from PascalSCADA project's serialport.pas file:
  // http://pascalscada.svn.sourceforge.net/viewvc/pascalscada/trunk/serialport.pas?revision=702&view=markup
  // More prefixes exist see e.g. :
  // http://comments.gmane.org/gmane.comp.ide.lazarus.general/46750

  {$IFDEF UNIX}
  var
    {$IFDEF LINUX}
    PortPrefix: array[0..1] of string = ({'ttyS',} 'ttyUSB', 'ttyACM');  // Possibly several other names too
    {$ENDIF}
    {$IFDEF FREEBSD}
    PortPrefix:array[0..2] of string = ('cuad', 'cuau', 'ttyu'); // http://www.freebsd.org/doc/handbook/serial.html
    {$ENDIF}
    {$IFDEF NETBSD}
    PortPrefix:array[0..0] of string = ('cuad');  // Don't have better info at the moment
    {$ENDIF}
    {$IFDEF OPENBSD}
    PortPrefix:array[0..0] of string = ('cuad');  // Don't have better info at the moment
    {$ENDIF}
  {$ENDIF}

  function GetSerialPortNames: string;
  var
    i: Integer;
    sr : TSearchRec;
    f: TSerialHandle;
    val: integer;
    ser: TSerialStruct;
  begin
    Result := '';
    try
      for i := 0 to high(PortPrefix) do
        if FindFirst('/dev/' + PortPrefix[i] + '*', LongInt($FFFFFFFF), sr) = 0 then  // from patch
        begin
          repeat
            if (sr.Attr and $FFFFFFFF) = Sr.Attr then
            begin
              begin
                f := FpOpen('/dev/'+sr.Name, O_RDWR or O_NOCTTY or O_NONBLOCK);
                if f <> 0 then
                begin
                  val := FpIOCtl(f, TIOCGSERIAL, @ser);
                  if (val <> -1) then
                    Result := Result + '/dev/' + sr.Name + ',';
                  SerClose(f);
                end;
              end;
            end;
          until FindNext(sr) <> 0;
        end;
    finally
      FindClose(sr);
      // remove trailing comma
      i := length(Result);
      if (i > 0) and (Result[i] = ',') then
        Result[i] := #0;
    end;
  end;
  {$ENDIF}
////////////////////////////////////////////////////////////
end.

