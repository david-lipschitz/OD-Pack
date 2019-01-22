unit ODDBCommon;
{ OD DataXChange Common Functions - A.G
  Version 5.0.0
  Copyright (c) 1998-2019
  Orbital Decisions:- My Power Station Technology (Pty) Ltd
  P.O.Box 1080, Milnerton 7435, South Africa
  components@mypowerstation.biz
  http://www.orbital.co.za/text/prodlist.htm

  ( + = Todo, - = Possible Issues, # = Just Done.  Now do documentation & checking )
  + Add "personalized" about box function.  Each component should be able to
    send it's name to be shown.  Current system is way too generalised.

  Use at your own risk!
}

{$INCLUDE ODDatXSet.pas}

interface
uses
  Winapi.Windows, SysUtils, System.Classes, Vcl.FileCtrl, Data.DB,
  Vcl.Dialogs;

type
  // Types of operations that may be performed on a text file when writing.
  // omError denotes the file is invalid, or some other error occurred whilst
  // accessing it.  omNotCreated means that the path and file name are valid,
  // but does not (yet) exist.
  TFileRights = (omReadWrite, omReadOnly, omNotCreated, omError);
  // Way in which a file will be opened for access.
  // Equivalent to Reset(), Append() and ReWrite()
  TFileAccessMode = (amAppend, amNone, amReadOnly, amReWrite);

  // Object error msg - shows comp Name + ClassName + Msg
  EODObjectError = class(TObject)
  public
    constructor Create(const Msg: String; ClassRef: TObject);
  end;

  // Component error msg - shows comp Name + ClassName + Msg
  EODComponentError = class(TObject)
  public
    constructor Create(const Msg: String; ClassRef: TComponent);
  end;

  EFileAccessModeError = class(EODComponentError);

  // Component error msg + passed windows error
  // Shows EComponent Error + Text passsed Windows Error code + passed Error Code
  EODPassedCompError = class(EODComponentError)
  public
    constructor Create(const Msg: String; ClassRef: TComponent; ErrorCode: Integer);
  end;

  // Record to store the start and ending positions of a 1D object (e.g String)
  TSEPos = record
    StartPos, EndPos : Integer;
  end;

  // Record to store date member values
  TDateMembers = record
    DayStr, MonthStr, YearStr : String;
    DayLen, MonthLen, YearLen : Integer;
  end;

  // Record to store date member positions.
  TDateMemberPos = record
    Day, Month, Year : TSEPos;      // Start and End Positions
    DayL, MonthL, YearL : Integer;  // Member lengths
  end;

  TCharSet = set of Char;

  // Function to compare two values
  TCmpFunc = function(Item1, Item2: Pointer): Integer;

  // Object for handling a file's attributes
  TODFileAttr = class(TObject)
  private
    FFileName : TFileName;
    function GetArchive: Boolean;
    function GetHidden: Boolean;
    function GetReadOnly: Boolean;
    function GetSystem: Boolean;
    function GetBits: Integer;
    procedure SetArchive(Value: Boolean);
    procedure SetHidden(Value: Boolean);
    procedure SetReadOnly(Value: Boolean);
    procedure SetSystem(Value: Boolean);
    procedure SetBits(Value: Integer);
  public
    constructor Create(FileName: TFileName);
    property ReadOnly : Boolean read GetReadOnly write SetReadOnly;
    property Hidden : Boolean read GetHidden write SetHidden;
    property Archive : Boolean read GetArchive write SetArchive;
    property System : Boolean read GetSystem write SetSystem;
    property Bits : Integer read GetBits write SetBits;
    property FileName : TFileName read FFileName;
  end;

  TODFile = class(TComponent)
  private
    function GetFileAttr: TODFileAttr;
    procedure SetFileAttr(Value: TODFileAttr);
  protected
    FFileOpen : Boolean;       // Is the file open
    FFileName : TFileName;     // File Path/Name
    FAttributes : TODFileAttr; // File Attribute object
    FFileAccess : TFileRights;     // What can be done to the file - Not the attributes
    FCanAppend : Boolean;      // If the file does not exist then it cannot be appended to
    procedure SetFileName(Value: TFileName); virtual;
    function Open: Boolean; virtual; abstract;
    function Close: Boolean; virtual; abstract;
    property FileName : TFileName read FFileName write SetFileName;
    property IsOpen : Boolean read FFileOpen;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateFile(AOwner: TComponent; FileName: TFileName); virtual;
    destructor Destroy; override;
    property Attributes : TODFileAttr read GetFileAttr write SetFileAttr;
  end;

  // Object for handling very high level Text File I/O
  TODTextFile = class(TODFile)
  private
    FTextFile : TextFile;           // Text file "Object"
    FAccessMode : TFileAccessMode;  // Mode to open text file
    procedure SetAccessMode(Value: TFileAccessMode);
    function GetEOF: Boolean;
  protected
    FRequestedAM : TFileAccessMode; // Requested file access mode - set when file opened
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateFile(AOwner: TComponent; FileName: TFileName); override;
    destructor Destroy; override;
    function Open: Boolean; override;
    function Close: Boolean; override;
    function ReadLine: String;
    procedure WriteVals(Values: array of const);
    procedure WriteLine(Line: String);
    property EOF: Boolean read GetEOF;
    property AccessMode: TFileAccessMode read FAccessMode;
    property ReqAccessMode: TFileAccessMode read FRequestedAM write FRequestedAM;
    property FileName;
    property IsOpen;
  end;

  // A nice object for logging exceptions - uses a text file to store exception
  // records in a comma delimited format - just use TODDBImporter to put them
  // in a table afterwards!
  TDBExceptionFile = class(TODTextFile)
  private
    FHeadersAdded: Boolean; // Have FieldNames and FieldTypes headers been added
    // Property access methods
    procedure SetReqAM(Value: TFileAccessMode);
  public
    constructor Create(AOwner: TComponent); override;
    function Open: Boolean; override;
    //function Close: Boolean; override;
    procedure AppendException(OriginalLine, ExceptionType, BadField, BadFieldValue: String; RecordNo: Integer);
    property ReqAccessMode: TFileAccessMode read FRequestedAM write SetReqAM;
  end;

  // General Procedure/Function Declarations
  procedure ReportError(Severity, ErrorConst: Integer; Cause, ObjectName, ProcName: String);
  procedure ClearStrArr(var StrArr: array of String);
  procedure LinearSort(var Arr: array of const; CmpFunc: TCmpFunc);
  function BuildRecordFromList(pRecord: TStringList; pSepType, pQuoteType: String): String;
  function FindStr(StrArray: array of String; StrToFind: String; CaseSensitive: Boolean): Integer;
  function GetFixedDateMembers(strFormat, strDate: String): TDateMembers;
  function GetDateMembers(strFormat, strDate: String): TDateMembers;
  function CheckFile(pFileName: TFileName): TFileRights;
  function EncloseInQuotes(pFieldData, pQuoteType: String): String;
  function ClearFields(DataSet: TDataSet): Integer;
  function CheckArrIndex(Arr: array of String; Index: Integer): Boolean;
  function VerifyFileName(FileName: String): Boolean;
  function MakeLangID(plID, slID : Integer): Integer;
  function GetDataTypeAsString(FieldType: TFieldType): String;

const
  ODProductName = 'DataXChange Component Suite';
  ODProductWebPage = 'http://www.orbital.co.za';
  ODRegisterURL = 'http://www.shareit.com/programs/137262.htm';
  ODSalesEMail = 'components@mypowerstation.biz';
  {$IFDEF Full}
    ODVersion = '1.4.2';
    DemoVersion = False;
    TrialVersion = False;
  {$ENDIF}
  {$IFDEF Demo}
    ODVersion = '1.4.2 (Demo)';
    DemoVersion = True;
    TrialVersion = False;
  {$ENDIF}
  {$IFDEF Trial}
    ODVersion = '1.4.2 (30 Day Trial)';
    TrialVersion = True;
    DemoVersion = False;
  {$ENDIF}
  DemoMaxRecords = 100;
  DemoMaxFields = 5;
  AlphaNumeric : TCharSet = ['a'..'z', 'A'..'Z', '0'..'9'];

  NumTypes = 23 - 1; { The number of data types (Zero based) }
  // Various data types this component may read and write - in string format
  // This array is is an exact replicant in string form of TFieldType (23)
  // Item 0 must always be ftUnknown, dtError etc.
  OrigDataTypeNames : array [0..NumTypes] of String = ('ftUnknown','ftString','ftSmallint',
  'ftInteger','ftWord','ftBoolean','ftFloat','ftCurrency','ftBCD','ftDate',
  'ftTime','ftDateTime','ftBytes','ftVarBytes','ftAutoInc','ftBlob','ftMemo',
  'ftGraphic','ftFmtMemo','ftParadoxOle','ftDBaseOle','ftTypedBinary','ftCursor');
  DataTypeNames : array [0..NumTypes] of String = ('Unknown','String','Smallint',
  'Integer','Word','Boolean','Float','Currency','BCD','Date',
  'Time','DateTime','Bytes','VarBytes','AutoInc','Blob','Memo',
  'Graphic','FmtMemo','ParadoxOle','DBaseOle','TypedBinary','Cursor');

  // Error severity constants
  ESevere = 990001;
  ERecoverable = 990002;
  EWarning = 990003;
  EUser = 990004;

  // Error message constants
  ECShareWriteDenied = 0001;
  ECShareAccessDenied = 0002;
  ECInvalidDateFormat = 0004;
  ECUnableToSetAttribute = 0005;
  ECUnableToReadAttribute = 0005;
  ECInvalidPath = 0006;
  ECInvalidFile = 0007;
  ECInvalidDir = 0008;
  ECCannotAppendToNonExistentFile = 0009;
  ECCannotReadFromNonExistentFile = 0010;
  ECCannotWriteToReadOnlyFile = 0011;
  ECConvertError = 0012;
  ECWriteDenied = 0013;
  ECReadDenied = 0014;

implementation

uses FireDAC.Comp.Client
{$IFDEF Full};
{$ELSE} , ODDemo; {$ENDIF}

procedure ReportError(Severity, ErrorConst: Integer; Cause, ObjectName, ProcName: String);
var
  SeverityTxt, ErrorTxt : String;

begin
  case Severity of
    ESevere : SeverityTxt := 'Severe internal error - Program restart required';
    ERecoverable : SeverityTxt := 'Internal error - It may be possible to continue normally';
    EWarning : SeverityTxt := 'Warning';
    EUser : SeverityTxt := 'User error';
  end;

  case ErrorConst of
    ECShareWriteDenied : ErrorTxt := 'Sharing Violation - Write Denied';
    ECShareAccessDenied : ErrorTxt := 'Sharing Violation - Access Denied';
    ECReadDenied : ErrorTxt := 'Insuficient Rights - Read Denied';
    ECWriteDenied : ErrorTxt := 'Insuficient Rights - Write Denied';
    ECInvalidDateFormat : ErrorTxt := 'Invalid Date Format Specifier';
    ECUnableToSetAttribute : ErrorTxt := 'Unable to set file attribute(s)';
    ECInvalidPath : ErrorTxt := 'Unable to get file attribute(s)';
    ECInvalidFile : ErrorTxt := 'Invalid File (Does not exist)';
    ECInvalidDir : ErrorTxt := 'Invalid Directory/Folder (Does not exist)';
    ECCannotAppendToNonExistentFile : ErrorTxt := 'Cannot append to a non-existent file';
    ECCannotReadFromNonExistentFile : ErrorTxt := 'Cannot read from a non-existent file';
    ECCannotWriteToReadOnlyFile : ErrorTxt := 'Cannot write to a read-only file';
    ECConvertError : ErrorTxt := 'Type conversion error';
  end;

//  raise MessageBox(0, StrToIn
end;

// Linear sorting procedure
procedure LinearSort(var Arr: array of const; CmpFunc: TCmpFunc);
begin
  // WIP
end;

// Function to return passed field type as a string
function GetDataTypeAsString(FieldType: TFieldType): String;
begin
  Result := DataTypeNames[ord(FieldType)];
end;

// This function returns the index of the first occurence of a string in an array of strings
function FindStr(StrArray: array of String; StrToFind: String; CaseSensitive: Boolean): Integer;
var
  LPos : Integer;
  LMin, LMax : Integer;

begin
  Result := -1;
  LMin := Low(StrArray);
  LMax := High(StrArray);

  // Do a case insensitive linear search
  if CaseSensitive = False then
  begin
    StrToFind := UpperCase(StrToFind);
    for LPos := LMin to LMax do
    begin
      if UpperCase(StrArray[LPos]) = StrToFind then
      begin
        Result := LPos;
        Break;  // Stop when found
      end;
    end;
  end
  else if CaseSensitive = True then
  begin
    for LPos := LMin to LMax do
    begin
      if StrArray[LPos] = StrToFind then
      begin
        Result := LPos;
        Break;  // Stop when found
      end;
    end;
  end;
end;

// Procedure to clear an array of strings
procedure ClearStrArr(var StrArr: array of string);
var
  ix : Integer;
  LMin, LMax : Integer;

begin
  LMin := Low(StrArr);
  LMax := High(StrArr);

  for ix := LMin to LMax do
    StrArr[ix] := '';
end;

// This function returns info on the validity and access rights for a file
function CheckFile(pFileName: TFileName): TFileRights;
var
  LFileAttr : Integer;

begin
  Result := omError;
  if FileExists(pFileName) then
  begin
    LFileAttr := FileGetAttr(pFileName);
    if ((LFileAttr and System.SysUtils.faReadOnly) = System.SysUtils.faReadOnly) then
      Result := omReadOnly
    else if not (LFileAttr and System.SysUtils.faReadOnly) = System.SysUtils.faReadOnly then
      Result := omReadWrite;
  end
  else if FileExists(pFileName) = False then
  begin
    // Check if the filename could be created - FileName must be valid.
    if VerifyFileName(pFileName) = True then
      Result := omNotCreated  // One up from omError - this filename is valid
    // If the file does not exist, then check if it can be created
    else if VerifyFileName(pFileName) = False then
      Result := omError;  // Mission Impossible - UNABLE, UNABLE, ERROR!!!
  end;
end;

// Simple function to enclose a string in a specified quote type
// Unlike AnsiQuotedStr, it does not double every other quote char
function EncloseInQuotes(pFieldData, pQuoteType: String): String;
begin
    // Enclose in the specified quote type - Open quotes
  Result := Result + pQuoteType;
  // Add the field data
  Result := Result + pFieldData;
  // Close quotes
  Result := Result + pQuoteType;
end;

// Function to take a TDataSet and set all the fields to NULL
// Returns -1 on error.  If the DataSet is a TQuery, then the function
// clears the parameters and not the fields (would do nothing)
function ClearFields(DataSet: TDataSet): Integer;
var
  ix : Integer;

begin
  try
//    if (DataSet.ClassName = 'TFDQuery') then
    if (DataSet is TFDQuery) then
    begin
      for ix := 0 to (TFDQuery(DataSet).ParamCount - 1) do
        TFDQuery(DataSet).Params[ix].Clear;
      Result := 0;
    end
    else begin
      for ix := 0 to (DataSet.FieldCount - 1) do
        DataSet.Fields[ix].Clear;
      Result := 0;
    end;
  except
    Result := -1;
  end;
end;

// This functions checks if the passed index is legal for the specified array
// Returns true if OK, false if illegal
function CheckArrIndex(Arr: array of String; Index: Integer): Boolean;
var
  LMin, LMax : Integer;

begin
  LMin := Low(Arr);
  LMax := High(Arr);
  if (Index >= LMin) and (Index <= LMax) then
    Result := True
  else Result := False;
end;

// Function to take a TStringList and treat every line as a field value - with
// the output being a single string.
function BuildRecordFromList(pRecord: TStringList; pSepType, pQuoteType: String): String;
var
  LPos : Integer;
begin
  // Insert all the fields except the last
  Result := '';
  for LPos := 0 to (pRecord.Count - 2) do
  begin
    // Add a separator
    Result := Result + EncloseInQuotes(pRecord[LPos], pQuoteType);
    Result := Result + pSepType;
  end;
  // Add the last field
  Result := Result + EncloseInQuotes(pRecord[pRecord.Count - 1], pQuoteType);
end;

// This function returns the date members form a date string where the width
// of each member is known.  i.e ddmmyyyy means that the date string MUST
// be 8 characters, and each member must fit in it's allocated space.
function GetFixedDateMembers(strFormat, strDate: String): TDateMembers;
type
  TStringInt = record
    SPos, EPos : Integer;
//    Str : String;                 // Value of member
  end;

var
  DayF, MonthF, YearF: TStringInt;
  CurPos : Integer;

begin
  strFormat := UpperCase(strFormat);

  // Store the DayF start pos
  DayF.SPos := Pos('D', strFormat);
  // Find the DayF end pos
  CurPos := DayF.SPos;
  while strFormat[CurPos] = 'D' do
    Inc(CurPos);
  DayF.EPos := CurPos;

  // Store the MonthF start pos
  MonthF.SPos := Pos('M', strFormat);
  // Find the MonthF end pos
  CurPos := MonthF.SPos;
  while strFormat[CurPos] = 'M' do
    Inc(CurPos);
  MonthF.EPos := CurPos;

  // Store the YearF start pos
  YearF.SPos := Pos('Y', strFormat);
  // Find the YearF end pos
  CurPos := YearF.SPos;
  while strFormat[CurPos] = 'Y' do
    Inc(CurPos);
  YearF.EPos := CurPos;

  Result.DayLen := DayF.EPos - DayF.SPos;
  Result.MonthLen := MonthF.EPos - MonthF.SPos;
  Result.YearLen := YearF.EPos - YearF.SPos;

  Result.DayStr := Copy(strDate, DayF.SPos, Result.DayLen);
  Result.MonthStr := Copy(strDate, MonthF.SPos, Result.MonthLen);
  Result.YearStr := Copy(strDate, YearF.SPos, Result.YearLen);
end;

// This function returns the date members from a date format with separatorsz
function GetDateMembers(strFormat, strDate: String): TDateMembers;
const
  Day = 1;
  Month = 2;
  Year = 3;

type
  TStringInt = record
    SPos, EPos, MCode : Integer;  // Start Pos, End Pos, Member Code
    Str : String;                 // Value of member
  end;

  TSIArr3 = array [1..3] of TStringInt;

var
   // The positions of the first occurences of the date formatting constants
  LDex, MemNo : Integer;
  SpecOrig, MemOrig : TSIArr3;  // Format Specifier Members, Date Members

begin
  // The minimum date format is d-m-y (two separators)
  // A separator must be a non-alphanumeric character
  // D = Day, M = Month, Y = Year
  strFormat := UpperCase(strFormat);
  // Break up the format string, and store each 3 members in their original order
  // Copy First 3 chars up to Separator of Format (Member

  // Store the start and ending positions of the date format members.
  LDex := 1;
  for MemNo := 1 to 3 do
  begin
  // Store the starting position of the current member
    SpecOrig[MemNo].SPos := LDex;
    while strFormat[LDex] in AlphaNumeric do
    begin
      // Store the member as a string
      SpecOrig[MemNo].Str := SpecOrig[MemNo].Str + strFormat[LDex];
      Inc(LDex);
    end;
    SpecOrig[MemNo].EPos := LDex - 1;
    // Get type of format member (Day, Month, Year) by checking first char
    case SpecOrig[MemNo].Str[1] of
      'D': SpecOrig[MemNo].MCode := Day;
      'M': SpecOrig[MemNo].MCode := Month;
      'Y': SpecOrig[MemNo].MCode := Year;
    end;
    // Skip over the separator (if any)
    while not (strFormat[LDex] in AlphaNumeric) do
      Inc(LDex);  // Skip one char
  end;

  // Copy the member codes of SpecOrig to MemOrig
  for MemNo := 1 to 3 do
    MemOrig[MemNo].MCode := SpecOrig[MemNo].MCode;

  // Store the start and ending positions of the date string members.
  LDex := 1;
  for MemNo := 1 to 3 do
  begin
    // Store the starting position of the current member
    MemOrig[MemNo].SPos := LDex;
    while strDate[LDex] in AlphaNumeric do
    begin
      // Store the member as a string
      MemOrig[MemNo].Str := MemOrig[MemNo].Str + strDate[LDex];
      Inc(LDex);
    end;
    MemOrig[MemNo].EPos := LDex - 1;
    // Skip over the separator (if any)
    while not (strDate[LDex] in AlphaNumeric) do
      Inc(LDex);  // Skip one char
  end;

  // Find and store the day member
  for MemNo := 1 to 3 do
    if MemOrig[MemNo].MCode = Day then
      Result.DayStr := MemOrig[MemNo].Str;
  // Find and store the month member
  for MemNo := 1 to 3 do
    if MemOrig[MemNo].MCode = Month then
      Result.MonthStr := MemOrig[MemNo].Str;
  // Find and store the year member
  for MemNo := 1 to 3 do
    if MemOrig[MemNo].MCode = Year then
      Result.YearStr := MemOrig[MemNo].Str;

  // Store date member lengths - best for conversion function to rely on this
  // rather than format member lengths - It just MIGHT vary ;^)
  Result.DayLen := Length(Result.DayStr);
  Result.MonthLen := Length(Result.MonthStr);
  Result.YearLen := Length(Result.YearStr);
end;

// Function checks if passed string is a legal filename
function VerifyFileName(FileName: String): Boolean;
var
  NotOK: Integer;
  SFileName: String;  // Stripped filename (no path)

begin
  // Check if the whole path contains any illegal codes
  // Cannot contain \ / : * ? " < > |
  Result := False;  // Guilty until proven innocent!
  NotOK := Pos('/', FileName);
  NotOK := NotOK or Pos('*', FileName);
  NotOK := NotOK or Pos('?', FileName);
  NotOK := NotOK or Pos('"', FileName);
  NotOK := NotOK or Pos('<', FileName);
  NotOK := NotOK or Pos('>', FileName);
  NotOK := NotOK or Pos('|', FileName);
  if NotOK = 0 then
    Result := True;
  // Just check if the fileName contains any illegal codes
  // Cannot contain \ / : * ? " < > |
  SFileName := ExtractFileName(FileName);
  NotOK := Pos('/', SFileName);
  NotOK := NotOK or Pos(':', SFileName);
  NotOK := NotOK or Pos('*', SFileName);
  NotOK := NotOK or Pos('?', SFileName);
  NotOK := NotOK or Pos('"', SFileName);
  NotOK := NotOK or Pos('<', SFileName);
  NotOK := NotOK or Pos('>', SFileName);
  NotOK := NotOK or Pos('|', SFileName);
  if NotOK = 0 then
    Result := True;
end;

// Function identical to C Win32 API macro MakeLangId
// plID = Primary Language ID, slID = Sub Language ID
function MakeLangID(plID, slID : Integer): Integer;
begin
  plID := plID shl 10;
  Result := plID or slID;
end;

//
// EODObjectError
//

constructor EODObjectError.Create(const Msg: String; ClassRef: TObject);
var
  Caption : String;

begin
  inherited Create;
  // Find out a way to get the name of the object that ClassRef is pointing to.
  // If a variable name stores a pointer to a memory address, then surely
  // it is possible to get the name of the variable that is pointing???
  Caption := ClassRef.ClassName + ' Exception ';
  MessageBox(0, PChar(Msg), PChar(Caption),  MB_OK or MB_ICONEXCLAMATION);
end;

//
// EODComponentError
//

constructor EODComponentError.Create(const Msg: String; ClassRef: TComponent);
var
  Caption : String;

begin
  inherited Create;
  Caption := ClassRef.ClassName + ' Exception '  + ' (' + ClassRef.Name + ')';
  MessageBox(0, PChar(Msg), PChar(Caption),  MB_OK or MB_ICONEXCLAMATION);
end;

constructor EODPassedCompError.Create(const Msg: String; ClassRef: TComponent; ErrorCode: Integer);
begin
  inherited Create(Msg + ' - ' + SysErrorMessage(ErrorCode) +
     ' - Error Code: ' + IntToStr(ErrorCode), ClassRef);
end;


{ TODFileAttr Object ----------------------------------------------------------}

constructor TODFileAttr.Create(FileName: TFileName);
begin
  inherited Create;
  FFileName := FileName;
{ faReadOnly	$00000001	Read-only files
  faHidden	$00000002	Hidden files
  faSysFile	$00000004	System files
  faVolumeID	$00000008	Volume ID files
  faDirectory	$00000010	Directory files
  faArchive	$00000020	Archive files }
end;

function TODFileAttr.GetArchive: Boolean;
var
  Bits : Integer;

begin
  Bits := GetBits;
  if Bits = -1 then
  begin
    Result := False;
    Exit;
  end;
  if Bits and SysUtils.faArchive <> 0 then
    Result := True
  else Result := False;
end;

function TODFileAttr.GetHidden: Boolean;
var
  Bits : Integer;

begin
  Bits := GetBits;
  if Bits = -1 then
  begin
    Result := False;
    Exit;
  end;
  if Bits and SysUtils.faHidden <> 0 then
    Result := True
  else Result := False;
end;

function TODFileAttr.GetReadOnly: Boolean;
var
  Bits : Integer;

begin
  Bits := GetBits;
  if Bits = -1 then
  begin
    Result := False;
    Exit;
  end;
  if Bits and SysUtils.faReadOnly <> 0 then
    Result := True
  else Result := False;
end;

function TODFileAttr.GetSystem: Boolean;
var
  Bits : Integer;

begin
  Bits := GetBits;
  if Bits = -1 then
  begin
    Result := False;
    Exit;
  end;
  if Bits and SysUtils.faSysFile <> 0 then
    Result := True
  else Result := False;
end;

function TODFileAttr.GetBits: Integer;
begin
  // Do not try to read attributes of a non-existent file
  if FileExists(FFileName) then
  begin
    Result := FileGetAttr(FFileName);
    if Result = -1 then
     raise Exception.Create('TODFileAttr - Could not set file attributes: '+
       SysErrorMessage(GetLastError) + ' - Error Code: ' + IntToStr(GetLastError));
  end
  else Result := -1;
end;

procedure TODFileAttr.SetArchive(Value: Boolean);
var
  Error : Integer;

begin
  // Do not try to set attributes for a non-existent file
  if FileExists(FFileName) then
  begin
   Error := FileSetAttr(FFileName, GetBits or SysUtils.faArchive);
   if Error <> 0 then
     raise Exception.Create('TODFileAttr - Could not set file attributes: '+
       SysErrorMessage(Error) + ' - Error Code: ' + IntToStr(Error));
  end;
end;

procedure TODFileAttr.SetHidden(Value: Boolean);
var
  Error : Integer;

begin
  // Do not try to set attributes for a non-existent file
  if FileExists(FFileName) then
  begin
   Error := FileSetAttr(FFileName, GetBits or SysUtils.faHidden);
   if Error <> 0 then
     raise Exception.Create('TODFileAttr - Could not set file attributes: '+
       SysErrorMessage(Error) + ' - Error Code: ' + IntToStr(Error));
  end;
end;

procedure TODFileAttr.SetReadOnly(Value: Boolean);
var
  Error : Integer;

begin
  // Do not try to set attributes for a non-existent file
  if FileExists(FFileName) then
  begin
   Error := FileSetAttr(FFileName, GetBits or SysUtils.faReadOnly);
   if Error <> 0 then
     raise Exception.Create('TODFileAttr - Could not set file attributes: '+
       SysErrorMessage(Error) + ' - Error Code: ' + IntToStr(Error));
  end;
end;

procedure TODFileAttr.SetSystem(Value: Boolean);
var
  Error : Integer;

begin
  // Do not try to set attributes for a non-existent file
  if FileExists(FFileName) then
  begin
   Error := FileSetAttr(FFileName, GetBits or SysUtils.faSysFile);
   if Error <> 0 then
     raise Exception.Create('TODFileAttr - Could not set file attributes: '+
       SysErrorMessage(Error) + ' - Error Code: ' + IntToStr(Error));
  end;
end;

procedure TODFileAttr.SetBits(Value: Integer);
var
  Error : Integer;

begin
  // Do not try to set attributes for a non-existent file
  if FileExists(FFileName) then
  begin
    // Raise an exception with the Windows error code and text explanation.
    Error := FileSetAttr(FFileName, Value);
    if Error <> 0 then
      raise Exception.Create('TODFileAttr - Could not set file attributes: '+
        SysErrorMessage(Error) + ' - Error Code: ' + IntToStr(Error));
  end
end;

{ TODFile Object --------------------------------------------------------------}

constructor TODFile.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFileName := '';
  FFileAccess := omError;
  FAttributes := TODFileAttr.Create(FFileName);
end;

constructor TODFile.CreateFile(AOwner: TComponent; FileName: TFileName);
begin
  Create(AOwner);
  // Recreate FAttributes with passed filename
  FAttributes.Free;
  FAttributes := TODFileAttr.Create(FileName);
  // Check and set filename
  SetFileName(FileName);
end;

destructor TODFile.Destroy;
begin
  // Free all dynamic resources
  FAttributes.Free;
  inherited Destroy;
end;

function TODFile.GetFileAttr: TODFileAttr;
begin
  Result := FAttributes;
end;

procedure TODFile.SetFileAttr(Value: TODFileAttr);
begin
  if Assigned(Value) then
  begin
    FAttributes.Free;
    FAttributes := Value;
  end;
end;

procedure TODFile.SetFileName(Value: TFileName);
begin
  // Make sure that the file's attributes and access type is known
  if FileExists(Value) then
  try
    FAttributes.Free;
    FAttributes := TODFileAttr.Create(Value);
    if FAttributes.GetBits = -1 then
      FFileAccess := omError
    else if FAttributes.ReadOnly then
      FFileAccess := omReadOnly
    else if not FAttributes.ReadOnly then
      FFileAccess := omReadWrite;
  except
    raise EODComponentError.Create('SetFileName Error (File exists, yet cannot be accessed - Possible sharing violation)', Self);
  end
  else if not FileExists(Value) then
  try
  //begin
  // If the file does not exist, then check if it can be created
    if VerifyFileName(Value) = True then
      FFileAccess := omNotCreated  // One up from omError - this filename is valid
    else
      FFileAccess := omError;
  except
    FFileAccess := omError;
    //raise EODComponentError.Create('SetFileName Error (File Invalid)', Self);}
  end;
  FFileName := Value;  // Only gets set if there were no exceptions
end;

{ TODTextFile Object ----------------------------------------------------------}

constructor TODTextFile.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // amReWrite is the safest default for the access mode
  FAccessMode := amReWrite;
  FRequestedAM := amReWrite;
end;

constructor TODTextFile.CreateFile(AOwner: TComponent; FileName: TFileName);
begin
  inherited CreateFile(AOwner, FileName);
  // amReWrite is the safest default for the access mode
  FAccessMode := amReWrite;
  FRequestedAM := amReWrite;
end;

destructor TODTextFile.Destroy;
begin
  if FFileOpen then Close;
  inherited Destroy;
end;

procedure TODTextFile.SetAccessMode(Value: TFileAccessMode);
begin
  // Do not allow anything except amReWrite for a file that does not exist
  if (FFileAccess = omError) and (Value <> amNone) then
  begin
    FAccessMode := amNone;
    EFileAccessModeError.Create('Unable to access file "' + FileName + '" - invalid file path!', Self);
  end
  // Cannot be read from OR appended to if it does not exist
  else if (FFileAccess = omNotCreated) and (Value = amReadOnly) then
  begin
    FAccessMode := amReWrite;
    EFileAccessModeError.Create('Unable to read from file "' + FileName + '" - does not exist!', Self);
  end
  else if (FFileAccess = omNotCreated) and (Value = amAppend) then
  begin
    FAccessMode := amReWrite;
    EFileAccessModeError.Create('Unable to append to file "' + FileName + '" - does not exist!', Self);
  end
  // Can only be read from if read-only
  else if (FFileAccess = omReadOnly) and ((Value = amRewrite) or (Value = amAppend)) then
  begin
    FAccessMode := amReadOnly;
    EFileAccessModeError.Create('Unable to write to file "' + FileName + '" - read only!', Self);
  end
  // File can only be appended to if read/write + exists, therefore,
  // anything goes if the file is Read/Write + Exists
  else if FFileAccess = omReadWrite then
    FAccessMode := Value
  // Anything goes in wierd scenario
  else FAccessMode := Value;
end;

// Function to open the file in the desired mode
function TODTextFile.Open: Boolean;
begin
  Result := False;
  try
    SetAccessMode(FRequestedAM);
    if FFileOpen then
      CloseFile(FTextFile);
    AssignFile(FTextFile, FFileName);
    if FAccessMode <> amNone then
    begin
      if FAccessMode = amReadOnly then
        Reset(FTextFile)
      else if FAccessMode = amAppend then
        Append(FTextFile)
      else if FAccessMode = amReWrite then
        Rewrite(FTextFile);
      FFileOpen := True;
      Result := True;
    end;
  except
    CloseFile(FTextFile);
    FFileOpen := False;
  end;
  // Update the file access mode.  This is important, as the file may not have
  // existed (omNotCreated).  Now the file exists, but FFileAccess is still
  // omNotCreated.  So update FFileAccess if it does exist.
  if FileExists(FFileName) then
  begin
    // It exits, now determine if it is writable.
    if Self.Attributes.ReadOnly then
      FFileAccess := omReadOnly
    else
      FFileAccess := omReadWrite;
  end;
end;

// Function to close the file and free any allocated resources
function TODTextFile.Close: Boolean;
begin
  CloseFile(FTextFile);
  FFileOpen := False;
  Result := True;
end;

function TODTextFile.ReadLine: String;
begin
  ReadLn(FTextFile, Result);
end;

function TODTextFile.GetEOF: Boolean;
begin
  Result := System.Eof(FTextFile);
end;

// Function to write an array of heterogeneous types
procedure TODTextFile.WriteVals(Values: array of const);
const
  BoolChars: array[Boolean] of Char = ('F', 'T');

var
  Max, ix : Integer;

begin
  if FAttributes.ReadOnly then
    raise EODComponentError.Create('Cannot write to read-only file!', Self);
  Max := High(Values);
  // Write all the passed values to the text file
  for ix := 0 to Max do
    with Values[ix] do
      case VType of
        vtInteger:    Write(FTextFile, IntToStr(VInteger));
        vtBoolean:    Write(FTextFile, BoolChars[VBoolean]);
        vtChar:       Write(FTextFile, VChar);
        vtExtended:   Write(FTextFile, FloatToStr(VExtended^));
        vtString:     Write(FTextFile, VString^);
        vtPChar:      Write(FTextFile, VPChar);
        vtObject:     Write(FTextFile, VObject.ClassName);
        vtClass:      Write(FTextFile, VClass.ClassName);
        vtAnsiString: Write(FTextFile, string(VAnsiString));
        vtCurrency:   Write(FTextFile, CurrToStr(VCurrency^));
        vtVariant:    Write(FTextFile, string(VVariant^));
        {$IFDEF VER130}
        vtInt64:      Write(FTextFile, IntToStr(VInt64^));
        {$ENDIF}
    end;
end;

// Like WriteVals + writes EOL marker
procedure TODTextFile.WriteLine(Line: String);
begin
  if FAttributes.ReadOnly then
    raise EODComponentError.Create('Cannot write to a read-only file!', Self);
  WriteLn(FTextFile, Line);
end;

{ TDBExceptionFile Object -----------------------------------------------------}

constructor TDBExceptionFile.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHeadersAdded := False;
end;

function TDBExceptionFile.Open: Boolean;
begin
  Result := inherited Open;
  if Result = True then
  begin
    // If the file was opened in append mode, then assume that the header records
    // already exist.
    if FRequestedAM = amAppend then
      FHeadersAdded := True
    // Else create the header records if not in Append mode
    else if FRequestedAM = amReWrite then
    begin
      // Create the header records
      WriteLine('RecordNo,DateTime,BadField,BadFieldValue,ExceptionType');
      WriteLine('Integer,TDateTime,String,BadFieldValue,String');
      FHeadersAdded := True;
    end;
  end;
end;

// Procedure to append an exception record to the exception report
procedure TDBExceptionFile.AppendException(OriginalLine, ExceptionType, BadField, BadFieldValue: String; RecordNo: Integer);
begin
  // Open the file and add the headers if they are not already there
  if (not FFileOpen) and (not FHeadersAdded) then
  begin
    Self.Open;
    // File should be open at this point.  Make sure all OK
    // By this stage, the headers should be in if they weren't to start with
    Self.Close;
  end;
  // Re-open it in amAppend mode
  FRequestedAM := amAppend;
  if Self.Open then
  begin
    // Log the exception in the following format:
    // --RecordNo, DateTime, BadField, BadFieldValue, ExceptionType--
    // --Integer, TDateTime, String, String, String--
    WriteLine(IntToStr(RecordNo) + ',' + DateTimeToStr(Now) + ',' +
      BadField + ',' + BadFieldValue + ',' +
      ExceptionType);
    // I think it might be better to store the comment as a separate field as
    // above as this would allow someone to treat the original record as a
    // field - perfect for a database table.  This would require the parsing
    // engine to be able to recognise data preceded by the comment string half
    // way.  Mucho complicated stuff.  This would mean the necescity of creating
    // a compiler style parser with more than just a couple of options.  All
    // together, a major step forward.
    WriteLine('//' + OriginalLine);
    // Close the file to ensure an update
    Self.Close;
  end
  else raise EODComponentError.Create('An error occured whilst trying to log an ' +
  'exception: ' + #13 + 'The exception file could not be opened for appending.', Self);
end;

procedure TDBExceptionFile.SetReqAM(Value: TFileAccessMode);
begin
  // Make sure that the access mode cannot be set to amReadOnly or amNone
  if (Value = amReadOnly) or (Value = amNone) then
    raise Exception.Create('The access mode must be either amReWrite or amAppend!')
  else FRequestedAM := Value;
end;

end.



