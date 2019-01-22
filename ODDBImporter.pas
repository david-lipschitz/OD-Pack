unit ODDBImporter;

{ TODDBImporter - By Orbital Decisions South Africa
                  Written by Alan Givati
  Copyright (c) 1998-2019
  Orbital Decisions:- My Power Station Technology (Pty) Ltd
  P.O.Box 1080, Milnerton 7435, South Africa
  components@mypowerstation.biz
  http://www.orbital.co.za/text/prodlist.htm

  Version 1.4.2 - Compatible With Delphi XE8
  Version 5.0.0 - Compatible With Delphi 10.3 Rio

  Use at your own risk!

  ( + = Todo, - = Possible Issues, # = Just Done.  Now do documentation & checking )
  Urgent - + Convert fractional values when using AsInteger to 0.
           + Update TODField.AsDateTime.  This function currently only handles
             date fields.  It will be expanded to handle Hours, Minutes and
             Seconds later in either 24hr or 12hr formats.
           + Add ability for user to choose which (if any) exception types are
             to be logged.
           + Refurbish to use TODTextFile instead of TODDBImporter managed
             TextFile "object".  Cleaner code and more managable.
           + Use planned advanced code exception reporting...
           + Do more double-quote testing (Works 100% so far) but the entire
             MakeRecord function could probably be optimised quite a bit more.
           + Could probably be optimized a bit more??? ;-)
           + Date conversions do not work properly if long months are used
           + Add Array sorting function - shame on Borland for not providing any!
           + Add "AllowEscapeChars" property to allow \" to act as a
             quote as in C/C++'s printf() as well as all other standard Esc sequences.
           - The ODDBCommon story.  HeadersUsed and HeadersToImport was:
             FieldNames, FieldTypes, FieldLengths.  Now they are different:
               imFieldNames, imFieldTypes, imFieldLengths for TODDBImporter
               exFieldNames, exFieldTypes, exFieldLengths for TODDBExporter
             FieldNames, FieldTypes, etc. still gets written/read from a file's header.
           - Add better* exception logging.  *Is it possible to be any better?
             This would involve the following possible additions:
               + Ability to log to BDE table of user choice.
               + Ability to auto-create this table if it does not exist.
               + Primary Key would have to be on RecordNumber and LogTime.
               + Ability to log to Paradox table without BDE???
           - Problem with the last record having EOF char instead of $0A
             This means I won't be able to use Readln anymore! - Fixed (So far)
             I would like to create my own Readln equivalent that could be set
             to stop at a user specified String.  This would enable Unix
             compatiability and make it more like the Oracle table loader.
           - Is it a good idea to auto-open the file if Next is called and
             it isn't already open?   This is currently disabled.
           - Should Importer be autoclosed if already open?  (Enabled)
           # Allow for comments (where the first 2 chars on a line are //)
             In this way the entire line would be skipped (totally ignored)
             The Next function would move forward until a proper record is
             found.  RecNo would not be incremented on such a comment, CurLineNo
             would be, however.  Also, provide property like SkipComments
             If SkipComments then treat <<< preceded lines as normal.

====================================================================================
  Conventions (Probably on the way out as they are non-standard) :

  PValue - prefix 'P' added to all types and vars of pointer type.
  pValue -  prefix 'p' added to all procedure and function parameters
  LVar -  prefix 'l' added to all variables local to a procedure or function
  FVar - prefiex 'F' added to all private class variables - The only standard convention

  The Words "Delimiter" and "Field Separator" are used interchangably!
    SO DONT GET CONFUSED!!!

1.  Find out the fastest way to get the number of LINES in a text file.
2.  Find out the fastest way to get the number of records that a SQL query returns.
    The fastest solution is to add a count column to the end of the query.  This
    will require a full parser...  Not so important right now.
}

// Include the DCR file with the components icon... Not Required for D4+
// Unfortunately Delphi 3 does not recognise the DCR file in some instances...
//{$INCLUDE ODDBImporter.dcr}

interface

{$INCLUDE ODDatXSet.pas}

uses
  Data.DB, System.Classes, System.SysUtils, Vcl.FileCtrl,
  {$IFDEF Full} ODDBCommon;
  {$ELSE} ODDBCommon, ODDemo; {$ENDIF}

type
  { Types of imports (itDelimited is VALUE1;VALUE2;etc, itFixed is FixedFormat)}
  TImportType = (itDelimited, itFixed);

  TDateFormatType = (dfFixed, dfSeparated);

  TString256 = array [0..255] of String;

  { This code was shared by ODDBCommon }
  { Types of header records - Field Names, Field Types and Field Lengths }
  THeaderType = (imFieldNames, imFieldTypes, imFieldLengths);
  THeaderTypes = set of THeaderType;

  // Stores an array of 256 Strings as well as the number of Strings used
  TStringRecord = record
    Strings : TString256;
    FieldCount : Integer;
  end;

  EFieldReturnError = class(EODComponentError)
  public
    FieldName : String;
    constructor Create(FName: String; ClassRef: TComponent);
  end;

(*  // Note: I have yet do define the import/export scripting language
  TODImportField = class(TObject)
  public
    SourceField : String;
    SourceDefualt : String;  // Currently this will only be used if the source is NULL
  { TargetDB : String;         // The target database name/ BDE alias (Must exist)
    TargetDBUN : String;       // The target DB's User Name
    TargetDBPW : String;       // The target DB's Password }
    TargetTable : String;      // The target table of the database (Must exist)
    { The target field name of the target table (Must exist) - in future versions change
    this to a TStringList to facilitate more than one target column }
    TargetField : String;
  end; *)

  // Class to manage a delimiter of variable length
  TDelimiter = class(TObject)
  private
    FDelimStart : Integer;
    FDelimSize : Integer;
    function GetDelimEnd: Integer;
  public
    property DelimStart : Integer read FDelimStart write FDelimStart;
    property DelimEnd : Integer read GetDelimEnd;
    constructor Create(StartPos: Integer; DelimSize: Integer);
  end;

  // TObjectList - a Base class for storing TObjects in a list.
  TObjectList = class(TList)
  private
    { Private declarations }
    FObjectCount : Integer;   // How many objects there are in this "String" of TObjects
  public
    { Public declarations }
    destructor Destroy; override;
    procedure Delete(pIndex: Integer);
  protected
    { Protected declarations }
    // CAdd and CInsert provide full functionality, however decendants must pass
    // an Object e.g. CAdd(TImportColumn.Create)
    procedure CAdd(pObject: TObject);
    procedure CInsert(pIndex: Integer; pObject: TObject);
  published
    { Published declarations }
    property ObjectCount : Integer read FObjectCount;
  end;

  // Object to manage the storage of delimiter objects
  TDelimList = class(TObjectList)
  private
    FDelimSize : Integer;
    function GetDelimiter(pIndex: Integer): TDelimiter;
    procedure SetDelimiter(pIndex: Integer; pValue: TDelimiter);
  public
    constructor Create(pDelimSize: Integer);
    procedure AddDelim(pStartPos: Integer);
    property Delimiters[Index: Integer]: TDelimiter read GetDelimiter write SetDelimiter; default;
  end;

  // Object to hold & convert field data to various types
  TODField = class(TObject)
  private
    FField: String;            // Field data is stored internally as a String
    FFType: TFieldType;        // Stores the field type
    FFTypeStr: String;         // Stores the field type as a String
    FFieldName: String;        // Stores the field's name
    FRecordNo: Integer;        // Stores the record number
    FFieldNo: Integer;         // Stores the field's actual index
    FDateFormat: String;       // The Date format of the import field if Applicable
    FNull: Boolean;            // Is the field NULL?
    FDateConversionType: TDateFormatType;
    FDBEF: TDBExceptionFile;   // Pointer to exception file object
    FOrigRec: String;          // Original record (full line)
    FLogExceptions: Boolean;   // Should an exception be logged
    FRaiseExceptions: Boolean; // Should User-Level exceptions be raised
    // Field type conversion functions
    function GetStrFromFType(pFieldType: TFieldType): String;
    function GetFTypeFromStr(pFieldType: String): TFieldType;
    // General Functions
    procedure SetDateFormatType;
    // Public Property fuctions
    function CheckIfNull: Boolean;
    function GetAsString: String;
    function GetAsInteger: Integer;
    function GetAsBoolean: Boolean;
    function GetAsFloat: Double;
    function GetAsDateTime: TDateTime;
    function GetAsTrimString: String;
    function GetDBEF: TDBExceptionFile;
    // Public Property methods
    procedure SetAsString(Value: String);
    procedure SetAsInteger(Value: Integer);
    procedure SetAsBoolean(Value: Boolean);
    procedure SetAsFloat(Value: Double);
    procedure SetAsDateTime(Value: TDateTime);
    procedure SetFType(Value: TFieldType);
    procedure SetFTypeStr(Value: String);
    procedure SetDBEF(Value: TDBExceptionFile);
  public
    constructor Create(RecordNo, FieldNo: Integer; DefDateFormat: String);
    function GetFromCustomDate(DateFormat: String): TDateTime;
    property AsTrimString: String read GetAsTrimString;
    property AsString: String read GetAsString write SetAsString;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property FieldType: TFieldType read FFType write SetFType;
    property FieldTypeStr: String read FFTypeStr write SetFTypeStr;
    property FieldName: String read FFieldName write FFieldName;
    property FieldNo: Integer read FFieldNo;
    property RecordNo: Integer read FRecordNo;
    property IsNull: Boolean read CheckIfNull;
    property OrigRec: String read FOrigRec write FOrigRec;
    property DBEF: TDBExceptionFile read GetDBEF write SetDBEF;
    property RaiseExceptions: Boolean read FRaiseExceptions write FRaiseExceptions;
  end;

  TODDBImporter = class(TComponent)
  private
    { Private declarations }
    { Non-Object members }
    FQuoteType: Char;
    {$IFDEF Trial}
    FDemoMRWShown: Boolean;      // Demo message check - see .Next()
    FStopRunning: Boolean;       // Set to True if Demo or Trial Version caught running outside Delphi
    FTrialExpired: Boolean;      // Set to True if the Trial version expired the trial period
    {$ENDIF}
    FNullInt: Integer;           // This is used to make read-only properties visible
    FFieldSeparator: String;     // Character/s used to separate fields in a record
    FRecValid: Boolean;          // Is the number of fields of the current record correct?
    FHeadersUsed: THeaderTypes;  // What types of hfields are there
    FImportFName: TFileName;     // The file from which to import incl. full path
    FCImpFile: TextFile;         // The current import textfile "object"
    FCDateFormat: String;        // The default custom date format
    FOrigRec: String;            // The original record, before field discovery
    FRecNo: Integer;             // The current record number
    FRecCount: Integer;          // TOTAL number of records excluding header
    FLineCount: Integer;         // Number of lines in the file
    FLineNo: Integer;            // The current line number in the import file
    FUseCtrlFile: Boolean;       // Should an external control file be used to get the headers?
    FCtrlFile: TODTextFile;      // The external control file (used to store headers)
    FCtrlFileName: TFileName;    // Local file name - gets streamed to DFM
    FImportType: TImportType;    // What type of import is to be done (decided at Open)
    FUserImpType: TImportType;   // What the user wants the import type to be
    FAllowComments: Boolean;     // Should lines preceded by FCommentString be skipped
    FCommentString: String;      // The comment type (default "//")
    FCommentLen: Integer;        // The number of chars in FCommentString
    FRaiseExceptions: Boolean;   // Should User-Level exceptions be raised
    // Is an Import File assigned - False if an attempt to open an invalid file is made
    FFileAssigned: Boolean;
    FOpen: Boolean;       // Is the import file open???
    // This variable is used to store the current import options in the DFM file
    FImportQryText: TStringList;
    // Exception logging related
    FExceptionFile: TDBExceptionFile;
    FExceptionLogging: Boolean;  // Enable exception logging
    { General Functions }
    function GetTFNumLines(pFileName: String; SkipComments, ValidateRecords: Boolean): Integer;
    function MakeRecordFixed(pRawRec: String): TStringRecord;
    // These functions are in order - MakeRecord up to StripQuotes are called by Next;
    function MakeRecord(pRawRec: String): TStringRecord;
    function StripQuotes(pQuoteType: Char; pMadeRec: TString256; pFieldCount: Integer): TString256;
    function NextHeader: TStringRecord;
    { Functions for properties }
    function GetEOF: Boolean;
    function GetHeaderFN(Index: Integer): String;
    function GetRecField(Index: Integer): String;
    function GetExceptionFName: TFileName;
    function GetExceptionLogging: Boolean;
    function GetExceptionLogMode: TFileAccessMode;
    function GetCtrlFName: TFileName;
    function GetAbout: String;
    { Procedures for properties }
    procedure SetImportFName(Value: TFileName);
    procedure SetExceptionFName(Value: TFileName);
    procedure SetExceptionLogging(Value: Boolean);
    procedure SetExceptionLogMode(Value: TFileAccessMode);
    procedure SetCDateFormat(Value: String);
    procedure SetHeaderFN(Index: Integer; Value: String);
    procedure SetCtrlFName(Value: TFileName);
    procedure SetAbout(Value: String);
    procedure SetCommentString(Value: String);
    // This is necessary to get property to work properly - see proc for more info
    procedure SetIQryText(pValue: TStringList);
  protected
    { Protected declarations }
    FHeaderFNRec : TStringRecord;    // Current import header (Field Names)
    FHeaderFTRec : TStringRecord;    // Current import header (Field Types)
    FHeaderFLRec : TStringRecord;    // Current import header (Field Lengths)
    FHeadersRead : THeaderTypes;     // List of headers read in
    FCurImportRec : TStringRecord;   // Current import record (Field Data)
    property CurImportRec[Index: Integer]: String read GetRecField;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Close;                    // Closes the file + clears up
    function First: Boolean;            // Reads the first record
    function GetRecordCount: Integer;   // Reads number of valid records in the import file
    function Open: Boolean;             // Sets up the import process - returns false if unable
    function Next: Boolean;             // Advance to the next textfile record
    function FieldByName(FieldName: String): TODField;
    function FieldByNameTrimmed(FieldName: String): TODField;
    function FieldByNumber(FieldNo: Integer): TODField;
    property CurFieldCount : Integer read FCurImportRec.FieldCount;
    property EOF : Boolean read GetEOF;
    property HeaderFNCount : Integer read FHeaderFNRec.FieldCount;
    property HeaderFNRec[Index: Integer]: String read GetHeaderFN write SetHeaderFN;
    property RecNo : Integer read FRecNo;
    property RecValid : Boolean read FRecValid;
  published
    { Published declarations }
    property About: String read GetAbout write SetAbout stored False;
    property AllowComments: Boolean read FAllowComments write FAllowComments;
    property ControlFile : TFileName read GetCtrlFName write SetCtrlFName;
    property CommentString: String read FCommentString write SetCommentString;
    property CustomDateFormat : String read FCDateFormat write SetCDateFormat;
    property ExceptionLogging : Boolean read GetExceptionLogging write SetExceptionLogging;
    property ExceptionLogMode : TFileAccessMode read GetExceptionLogMode write SetExceptionLogMode;
    property ExceptionFile : TFileName read GetExceptionFName write SetExceptionFName;
    property FieldSeparator : String read FFieldSeparator write FFieldSeparator;
    property HeadersUsed : THeaderTypes read FHeadersUsed write FHeadersUsed;
    property ImportQryText : TStringList read FImportQryText write SetIQryText;
    property ImportFile : TFileName read FImportFName write SetImportFName;
    property ImportType : TImportType read FUserImpType write FUserImpType;
    property QuoteType : Char read FQuoteType write FQuoteType;
    property RaiseExceptions: Boolean read FRaiseExceptions write FRaiseExceptions;
    property RecordCount : Integer read FRecCount write FNullInt;
    property UseControlFile : Boolean read FUseCtrlFile write FUseCtrlFile;
  end;

implementation

var Fmt: TFormatSettings;
//
// General Functions
//

function CheckIfRecord(Line: String): Boolean;
begin
  // A record is a non-blank line.
  // i.e. The line is blank if there are only whitespace/control characters
  if Trim(Line) = '' then
    Result := False
  else Result := True;
end;

//
// EFieldReturnError Implementation
//

constructor EFieldReturnError.Create(FName: String; ClassRef: TComponent);
begin
  inherited Create('Field ''' + FName + ''' not found.', ClassRef);
  FieldName := FName;
end;
//
// TFileProperty Implementation
//


//
// TDelimiter Implementation
//

function TDelimiter.GetDelimEnd: Integer;
begin
  // Function to simply return the end position of the delimiter based on it's size
  Result := FDelimStart + (FDelimSize - 1);
  // -1 to keep things right.  Otherwise a single char delimiter has an
  // ending pos greater than the the start position!!!
end;

constructor TDelimiter.Create(StartPos: Integer; DelimSize: Integer);
begin
  inherited Create;
  FDelimSize := DelimSize;
  FDelimStart := StartPos;
end;

//
// TObjectList Implementation - Stores TObjects in a row
//

destructor TObjectList.Destroy;
var
  LObj : Integer;

begin
  // Free all objects
  for LObj := 0 to (FObjectCount - 1) do
    TObject(Items[LObj]).Free;
  inherited Destroy;
end;

procedure TObjectList.CAdd(pObject: TObject);
begin
  { Adds a TObject based class to the list }
  { Increase the list's capacity to speed things up (I hope)}
  Capacity := Capacity + 1;   // Inc() doesn't work - huh???
  inherited Add(pObject);
  FObjectCount := Count;  // Let the world know that there is now one more object.
end;

procedure TObjectList.CInsert(pIndex: Integer; pObject: TObject);
begin
  { Insert a TObject into the list - shifts all objects up
    Increases capacity to speed up proccess.
    First check that the requested insertion point is not greater than the
    length of the Object list }
  if pIndex > Count then
  { Let it be known that a crime has been committed... }
    raise Exception.CreateFmt('Error - Cannot insert ' + pObject.ClassName +
    ' after last (%d) - %d', [FObjectCount-1, pIndex])
  { If there are no objects i.e the first object is being inserted then ADD one }
  else if (pIndex = -1) or (pIndex = Count) then
    CAdd(pObject)  // Call the new Add()
  // Make sure insertion point is valid i.e. within or equal to the count
  else if (pIndex <= (Count - 1)) and (pIndex > -1) then
  begin
    inherited Insert(pIndex, pObject);
    FObjectCount := Count;  // Let the world know that there is now one more object
  end;
end;

procedure TObjectList.Delete(pIndex: Integer);
begin
  { Delete an object based on index + Free associated memory
    First check that the index is valid! Remember that the index is zero based }
  if (pIndex >= Count) or (pIndex < 0) then
    raise Exception.CreateFmt('Cannot delete a cell that does not exist - cell (%d)', [pIndex])
  else if pIndex <= (Count - 1) then
  begin
    inherited Delete(pIndex);
    Capacity := Capacity - 1;  // Dammit - Inc is a blur
    FObjectCount := Count;  // Let the world know that there is now one less object
  end;
end;

//
// TDelimiList Implementation - A list of TDelimiters
//

function TDelimList.GetDelimiter(pIndex: Integer): TDelimiter;
begin
  // Make sure index is valid
  if (pIndex < 0) or (pIndex > (Count - 1)) then
    raise Exception.CreateFmt('Unable to retrieve delimiter (%d) - invalid index!', [pIndex]);
  Result := Items[pIndex]
end;

procedure TDelimList.SetDelimiter(pIndex: Integer; pValue: TDelimiter);
begin
  // Make sure index is valid
  if (pIndex > -1) and (pIndex < Count) then
    Items[pIndex] := pValue
  else
    raise Exception.CreateFmt('Unable to update delimiter (%d) - invalid index!', [pIndex]);
end;

constructor TDelimList.Create(pDelimSize: Integer);
begin
  inherited Create;
  FDelimSize := pDelimSize;
end;

procedure TDelimList.AddDelim(pStartPos: Integer);
begin
  inherited CAdd(TDelimiter.Create(pStartPos, FDelimSize));
end;

//
// TODField Implementation
//

constructor TODField.Create(RecordNo, FieldNo: Integer; DefDateFormat: String);
begin
  inherited Create;
  FFieldNo := FieldNo;
  FRecordNo := RecordNo;
  FField := '';
  FFieldName := '';
  // Set the default Custom Date Format - This may only contain Date format
  // specifiers and NOT time!
  FDateFormat := DefDateFormat;
  // Work out the type of date format
  SetDateFormatType;
  // Set some stuff needed for exception logging
  FLogExceptions := False;
  FOrigRec := '';
  FDBEF := nil;  // Pointer to exception logging file object
  // Should User-Level exceptions be raised.
  // A User-Level exception is one which the component can deal with.
  FRaiseExceptions := True;
end;

procedure TODField.SetDateFormatType;
const
  Alpha : TCharSet = ['a'..'z', 'A'..'Z'];

var
  Pos, FSL : Integer;

begin
  FSL := Length(FDateFormat);
  // Default is separated date members
  FDateConversionType := dfSeparated;
  // If there are only alpha chars then, it must be fixed.
  for Pos := 1 to FSL do
    if not (FDateFormat[Pos] in Alpha) then Exit;
  // If it got to here, then it must be fixed!
  FDateConversionType := dfFixed;
end;

function TODField.GetStrFromFType(pFieldType: TFieldType): String;
begin
  // Return the data type as a String
  Result := DataTypeNames[ord(pFieldType)];
end;

function TODField.GetFTypeFromStr(pFieldType: String): TFieldType;
var
  LCheck : Integer;

begin
  // This function converts a TFieldType to a String
  // Get the index of the passed String in the DataTypeNames array
  LCheck := FindStr(DataTypeNames, pFieldType, False);
  // Make sure that invalid type is never returned - on error return ftUnknown
  if LCheck = -1 then
    Result := ftUnknown
  else Result := TFieldType(LCheck);
end;

// Start of TODField conversion functions and procedures

// Public Property fuctions
function TODField.CheckIfNull: Boolean;
begin
  if FField = '' then Result := True
  else Result := False;
end;

function TODField.GetAsString: String;
begin
  Result := FField;
end;

// Return as a trimmed (leading and trailing whitespaces and system chars)
function TODField.GetAsTrimString: String;
begin
  Result := Trim(FField);
end;

function TODField.GetAsInteger: Integer;
begin
  if FNull then
    Result := 0
  else
  try
    Result := StrToInt(FField);
  except
    // Log the exception if exception logging is enabled
    if FLogExceptions and Assigned(FDBEF) then
      FDBEF.AppendException(FOrigRec, 'Get_AsInteger_Failed', FFieldName, FField, FRecordNo);
    // Raise an exception if required
    if FRaiseExceptions then
      raise EODObjectError.Create('Get_AsInteger_Failed for field "' + FFieldName +
      '" [Record ' + IntToStr(FRecordNo) + ']', Self)
    else Result := -1;
  end;
end;

function TODField.GetAsBoolean: Boolean;
begin
  if FNull then
    Result := False
  else if UpperCase(FField) = 'TRUE' then
    Result := True
  else if UpperCase(FField) = 'FALSE' then
    Result := False
  else begin
    // Log the exception if exception logging is enabled
    if FLogExceptions and Assigned(FDBEF) then
      FDBEF.AppendException(FOrigRec, 'Get_AsBoolean_Failed', FFieldName, FField, FRecordNo);
    // Raise an exception if required
    if FRaiseExceptions then
      raise EODObjectError.Create('Get_AsBoolean_Failed for field "' + FFieldName +
      '" [Record ' + IntToStr(FRecordNo) + ']', Self)
    else Result := False;
  end;
end;

function TODField.GetAsFloat: Double;
begin
  if FNull then
    Result := 0
  else try
    Result := StrToFloat(FField);
  except
    // Log the exception if exception logging is enabled
    if FLogExceptions then
      FDBEF.AppendException(FOrigRec, 'Get_AsFloat_Failed', FFieldName, FField, FRecordNo);
    // Raise an exception if required
    if FRaiseExceptions then
      raise EODObjectError.Create('Get_AsFloat_Failed for field "' + FFieldName +
      '" [Record ' + IntToStr(FRecordNo) + ']', Self)
    else Result := -1;
  end;
end;

// This function currently only handles data fields.  It will be expanded
// to handle Hours, Minutes and Seconds later in either 24hr or 12hr formats.
function TODField.GetAsDateTime: TDateTime;
begin
  if FNull then
    Result := 0
  else try
    Result := GetFromCustomDate(FDateFormat);
  except
    // Log the exception if exception logging is enabled
    if FLogExceptions then
      FDBEF.AppendException(FOrigRec, 'Get_AsCustomDate_Failed', FFieldName, FField, FRecordNo);
    // Raise an exception if required
    if FRaiseExceptions then
      raise EODObjectError.Create('Get_AsCustomDate_Failed for field "' + FFieldName +
      '" [Record ' + IntToStr(FRecordNo) + ']', Self)
    else Result := -1;
  end;
end;

// Function returns a TDateTime - the source is read as the format specified
// by FDateTimeFormat
function TODField.GetFromCustomDate(DateFormat: String): TDateTime;
var
  LDateMembers : TDateMembers;
  LDayVal, LMonthVal, LYearVal : Integer;
  LDay, LMonth, LYear : String;
  LDex : Integer;

begin
  if FDateConversionType = dfFixed then
    LDateMembers := GetFixedDateMembers(DateFormat, FField)
  else LDateMembers := GetDateMembers(DateFormat, FField);

  //Copy to local vars (easier to work with at the moment)
  LDay := LDateMembers.DayStr;
  LMonth := LDateMembers.MonthStr;
  LYear := LDateMembers.YearStr;
  LDayVal := -1;
  LMonthVal := -1;
  LYearVal := -1;

  // Format for single or double char DAY
  if (LDateMembers.DayLen = 1) or (LDateMembers.DayLen = 2) then
    LDayVal := StrToInt(LDay);
  // Format for single or double char MONTH
  if (LDateMembers.MonthLen = 1) or (LDateMembers.MonthLen = 2) then
    LMonthVal := StrToInt(LMonth)
  // Format for short MONTH names
  else if LDateMembers.MonthLen = 3 then
  begin
    // Check if the month name is in ShortMonthNames
    LDex := FindStr(Fmt.ShortMonthNames, LMonth, False);
    if LDex > -1 then
      LMonthVal := LDex + 1;
  end
  // Format for long MONTH names - Works as of 31 August 2000!!!
  else if LDateMembers.MonthLen > 3 then
  begin
    // Check if the month name is in LongMonthNames
    LDex := FindStr(Fmt.LongMonthNames, LMonth, False);
    if LDex > -1 then
      LMonthVal := LDex + 1;
  end;
  // Format for single or double char YEAR
  if (LDateMembers.YearLen = 1) or (LDateMembers.YearLen = 2) then
  begin
    LYearVal := StrToInt(LYear);
    // Make it Y2K compliant?!!!
    LYearVal := LYearVal + 2000;
  end;
  // Format for quad char YEAR (BEST!)
  if LDateMembers.YearLen = 4 then
    LYearVal := StrToInt(LYear);
  // Check if the year is zero - make it 2000 if it is
  // Millenium specific code:  Valid until the year 2999
  if LYearVal = 0 then
    LYearVal := 2000;
  Result := EncodeDate(Word(LYearVal), Word(LMonthVal), Word(LDayVal));
end;

// Public Property methods
procedure TODField.SetAsString(Value: String);
begin
  FField := Value;
  FNull := CheckIfNull;
end;

procedure TODField.SetAsInteger(Value: Integer);
begin
  FField := IntToStr(Value);
  FNull := CheckIfNull;
end;

procedure TODField.SetAsBoolean(Value: Boolean);
begin
  if Value = True then
    FField := 'True'
  else if Value = False then
    FField := 'False';
  FNull := CheckIfNull;
end;

procedure TODField.SetAsFloat(Value: Double);
begin
  FField := FloatToStr(Value);
  FNull := CheckIfNull;
end;

procedure TODField.SetAsDateTime(Value: TDateTime);
begin
  FField := DateTimeToStr(Value);
  FNull := CheckIfNull;
end;

// Set the field, AND store the field type as a String
procedure TODField.SetFType(Value: TFieldType);
begin
  FFType := Value;
  FFTypeStr := GetStrFromFType(FFType);
end;

// Set the field type from a String - convert to a TFieldType
procedure TODField.SetFTypeStr(Value: String);
begin
  FFType := GetFTypeFromStr(Value);
  // Re-Assign using conversion function in case of unknown type
  FFTypeStr := GetStrFromFType(FFType);
end;

function TODField.GetDBEF: TDBExceptionFile;
begin
  // Return nothing if pointer is invalid
  if Assigned(FDBEF) then
    Result := FDBEF
  else Result := nil;
end;

procedure TODField.SetDBEF(Value: TDBExceptionFile);
begin
  // Set only if the object reference is OK
  if Assigned(Value) then
  begin
    FDBEF.Free;
    FDBEF := Value;
    FLogExceptions := True;
  end
  else FLogExceptions := False;
end;
//
// TODDBImporter Implementation
//

constructor TODDBImporter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // Unused at the moment
  FImportQryText := TStringList.Create;
  // So let it be known
  FImportQryText.Add('Currently unused - reserved for future expansion.');
  FFieldSeparator := ',';
  FQuoteType := '"';
  FCurImportRec.FieldCount := 0;
  FHeaderFNRec.FieldCount := 0;
  FCDateFormat := 'dd-mm-yyyy';
  FCtrlFile := TODTextFile.Create(Self);
  FCtrlFileName := '';
  {$IFDEF Trial}
    FStopRunning := False;
    FDemoMRWShown := False;
  {$ENDIF}
  // Default exception options
  FExceptionLogging := False;
  FExceptionFile := TDBExceptionFile.Create(Self);
  // Should User-Level exceptions be raised.
  // A User-Level exception is one which the component can deal with.
  FRaiseExceptions := True;
  // Comment related
  FAllowComments := True;
  FCommentString := '//';
  // Do full-version checking
  {$IFDEF Full}
  {$ELSE}
    if (DemoVersion = True) and (DelphiLoaded = False) then
      FStopRunning := True;
    if (TrialVersion = True) and (DelphiLoaded = False) then
      FStopRunning := True;
  {$ENDIF}
  {$IFDEF Trial}
    FTrialExpired := CheckTrial(Self);
  {$ENDIF}
end;

destructor TODDBImporter.Destroy;
begin
  // End the import in case one is unclosed
  Self.Close;
  // Get rid of everything that shouldn't be!
  FImportQryText.Free;
  FCtrlFile.Free;
  FExceptionFile.Free;
  inherited Destroy;
end;

//
// General Functions
//

// This function counts the number of lines in a text file
// ValidateRecords should be true if the number of RECORDS needs to be gauged.
// A valid record is any non-comment/non-blank line.
// If SkipComments is True, then only RECORDS are counted (ValidateRecords is set to True)
// i.e. blank lines/invalid records are ALWAYS skipped if SkipComments is True
function TODDBImporter.GetTFNumLines(pFileName: String; SkipComments, ValidateRecords: Boolean): Integer;
var
  NumLines: Integer;
  TheTxtFile: TextFile;
  TempStr, LScan: String;
  IsRecord: Boolean;

begin
  NumLines := 0;
  // Open the specified file with read-only access.
  if FileExists(pFileName) and FFileAssigned then
  begin
    AssignFile(TheTxtFile, pFileName);
    Reset(TheTxtFile);
    try
      while not System.Eof(TheTxtFile) do
      begin
        ReadLn(TheTxtFile, TempStr);
        if SkipComments then
        begin
          // Get the first couple of chars (as many as the length of the comment string)
          // and compare it to the comment string.
          LScan := Copy(TempStr, 0, FCommentLen);
          // Check if the line is non-blank
          IsRecord := CheckIfRecord(TempStr);
          // Line count only gets inc'ed if it is a
          // valid (non-blank) record AND not a comment
          if IsRecord and (LScan <> FCommentString) then
            Inc(NumLines);  // As per normal
        end
        else
        begin
          if ValidateRecords then
          begin
            IsRecord := CheckIfRecord(TempStr);
            if IsRecord then
              Inc(NumLines);
          end
          else Inc(NumLines);
        end;
      end;
    finally
      CloseFile(TheTxtFile);  // Any exceptions are re-raised after here...
    end;
  end;
  Result := NumLines;
end;

// Function returns true if at the end of the file.
// This means that there are no more valid records after the current point.
// The file could however end 10 lines after the last valid record.
function TODDBImporter.GetEOF: Boolean;
begin
  // Old rotten code
//  Result := System.Eof(FCImpFile);
//              OR
{  if FLineNo > FLineCount then
    Result := True
  else Result := False; }
  if FRecNo > FRecCount then
    Result := True
  else Result := False;
end;

// This function opens the text file and gets it ready for importing line by line
function TODDBImporter.Open: Boolean;
  // function to check that FHeaderFLRec contains only UByte values
  function CheckFLHeader: Boolean;
  var
    ix : Integer;

  begin
    Result := True;
    for ix := 1 to FHeaderFLRec.FieldCount do
      if (StrToInt(FHeaderFLRec.Strings[ix-1]) > 255) or
        (StrToInt(FHeaderFLRec.Strings[ix-1]) < 0) then
        Result := False;
  end;

begin
  // Open the import file with read-only access.
  // Note:  If this file is called, the current import line will be set to ZERO!
  // Do not call this function once you have started importing records unless
  // you wish to start over...  You may call this function, even without calling
  // Close as it will always re-open the import file if one was already open.
  Result := False;
  // The current record number is updated every time a record is read
  FRecNo := 0;
  // The current line number is 0 - this is Inc'ed on EVERY ReadLn
  FLineNo := 0;
  {$IFDEF Demo}
    if FStopRunning = True then
    begin
      WarnTrial('Cannot be used outside Delphi!');
      Exit;
    end;
    // Make sure user is reminded that DEMO version is in use
    FDemoMRWShown := False;
  {$ENDIF}
  {$IFDEF Trial}
    if FStopRunning = True then
    begin
      WarnTrial('Cannot be used outside Delphi!');
      Exit;
    end;
    if FTrialExpired then
    begin
      WarnTrial('This trial version has expired!');
      Exit;
    end;
  {$ENDIF}
  FImportType := FUserImpType;  // Set the import type - unchanged until next open
  if FileExists(FImportFName) and FFileAssigned then
  try
    if FOpen then
      CloseFile(FCImpFile);
    AssignFile(FCImpFile, FImportFName);
    Reset(FCImpFile);
    FOpen := True;
    // Clear all headers and the "current" record
    FHeaderFNRec.FieldCount := 0;
    FHeaderFTRec.FieldCount := 0;
    FHeaderFLRec.FieldCount := 0;
    FCurImportRec.FieldCount := 0;
    ClearStrArr(FHeaderFNRec.Strings);
    ClearStrArr(FHeaderFTRec.Strings);
    ClearStrArr(FHeaderFLRec.Strings);
    ClearStrArr(FCurImportRec.Strings);
    // Store the total number of lines - this value may not be changed
    // Include comments and invalid records in the line count - if it is a line, it's in...
    FLineCount := GetTFNumLines(FImportFName, False, False);
    // Update the record count - Exclude comments from the line count if they are enabled
    // Also, exclude invalid records
    FRecCount := GetTFNumLines(FImportFName, FAllowComments, True);
    // Open the control file if one is used
    if FUseCtrlFile then
    begin
      FCtrlFile.ReqAccessMode := amReadOnly;
      if not FCtrlFile.Open then
        raise EODComponentError.Create('method "Open" - Could not open Control File for import!', Self);
    end;
    // Try to get the (header Field Names) record if selected
    if (imFieldNames in FHeadersUsed) then
    begin
      FHeaderFNRec := NextHeader;
      Include(FHeadersRead, imFieldNames);
      Dec(FRecCount);  // Exclude this header from the record count
    end;
    // Try to get the (header Field Types) record if selected
    if (imFieldTypes in FHeadersUsed) then
    begin
      FHeaderFTRec := NextHeader;
      Include(FHeadersRead, imFieldTypes);
      Dec(FRecCount);  // Exclude this header from the record count
    end;
    // Try to get the (header Field Lengths) record if selected OR if doing Fixed Width imports
    // The field widths MUST be present in the import file or the control file
    // if a fixed width import is being performed.  Faliure to comply with
    // this requirement will result in the file being closed
    if (imFieldLengths in FHeadersUsed) or (FImportType = itFixed) then
    begin
      FHeaderFLRec := NextHeader;
      if (CheckFLHeader = False) then
      begin
        Self.Close;
        Exit;
      end;
      Include(FHeadersRead, imFieldLengths);
      Dec(FRecCount);  // Exclude this header from the record count
    end
    else if not (imFieldLengths in FHeadersUsed) and (FImportType = itFixed) then
    begin
      Self.Close;
      Exit;
    end;
    // Close the control file if one is used + open
    if (FUseCtrlFile) and (FCtrlFile.IsOpen) then
      FCtrlFile.Close;
    Result := True;
    // FRecNo should be ZERO at this point
  except
    Self.Close;
  end
  else begin
    // The file was not found, so "raise" an exception
    EODComponentError.Create('File "' + FImportFName + '" not found.' + #13 +
      'The current directory is: "' + GetCurrentDir + '"', Self);
  end;
end;

// Simple function to end the import and reset the progess counter and record count
procedure TODDBImporter.Close;
begin
  if FOpen then
    CloseFile(FCImpFile);  // Any exceptions are re-raised after here
  FOpen := False;
  FHeaderFNRec.FieldCount := 0;
  FCurImportRec.FieldCount := 0;
  FRecNo := 0;
  FRecCount := 0;
end;

// Function takes a fixed format String and converts it to a TStringRecord
// The FHeaderFLRec header must have the appropriate Field Width imformation.
// Otherwise, the returned TStringRecord will be invalid.
// Returns blank if FHeadersUsed does not include FieldLengths or FHeaderFLRec
// has less than 1 fields.   Note:  A maximum of 255 records is imposed
function TODDBImporter.MakeRecordFixed(pRawRec: String): TStringRecord;
var
  LRecord : TStringRecord;
  Field, FPos : Integer;

begin
  Result := LRecord;
  FPos := 1;  // Position in raw record
  // Make sure all is in order before trying to extract the fields
  if not (imFieldLengths in FHeadersUsed) then Exit;
  if FHeaderFLRec.FieldCount < 1 then Exit;
  for Field := 1 to FHeaderFLRec.FieldCount do
  begin
    Result.Strings[Field-1] := Copy(pRawRec, FPos, StrToInt(FHeaderFLRec.Strings[Field-1]));
    FPos := FPos + StrToInt(FHeaderFLRec.Strings[Field-1]);
  end;
  // Set the field count of the String record
  Result.FieldCount := FHeaderFLRec.FieldCount;
end;

// This function takes a delimited String record and formats the result into
// a list of field values, accessible via their index.
// It's a pretty POWERFUL function, and can handle delimiters of any size,
// as well as quotes within quotes, and delimiters within those quotes.
// e.g. (Using commas (,) as delimiters)
//      "This is a test, ""time, flies fast""."
//    produces: This is a test, "time, flies fast". [single field]
//      """Van Der Merwe"" was quoted as saying that "Pigs, although heavy, can fly."""
//    produces: "Van Der Merwe was quoted as saying that "Pigs, although heavy, can fly."
function TODDBImporter.MakeRecord(pRawRec: String): TStringRecord;
var
  LDelimLen, LPos : Integer;
  //LField,
  LDelimNo, LRawRecLen, LFieldLen : Integer;
  LDelim : String;
  //LNewField : String;
  LDelims : TDelimList;
  LFields : TString256;
  // Some vars needed to check if a String has enclosing quotes
  LQuoteClosed : Boolean;
  // Checks for double quotes - need to test lots before sure - complicated!
  L2Quotes, LNext2 : String;

begin
  // Init
  LDelimLen := Length(FFieldSeparator);
  LRawRecLen := Length(pRawRec);
  LDelims := TDelimList.Create(LDelimLen);
  LPos := 1;
  L2Quotes := FQuoteType + FQuoteType;  // e.g. ""
  LQuoteClosed := True;
  Result.FieldCount := 1;  // There is ALWAYS at least one field...
  // End Init
  // Make sure an attempt is never made to format a record whose length is less
  // than that of the delimiter or less than 1 (the minimum, 1 field - 1 value)!
  if (LRawRecLen < 1) or (LRawRecLen < LDelimLen) then
  begin
    Result.Strings := LFields;
    Result.FieldCount := 0;
    Exit;
  end;
  // Find all the delimiters and build a list of them...
  // Loop through the String until the end is reached
  while LPos <= LRawRecLen do
  begin
    // Check if the next N (length of delimiter) character/s are the delimiter, and if so, store it!
    LDelim := Copy(pRawRec, LPos, LDelimLen);
    // Copy the next two chars to check if there are quote chars next to each other
    LNext2 := Copy(pRawRec, LPos, 2);
    // Check if the field is enclosed in quotes - if so, do not record delimiter
    // positions until a closing quote is found.
    // Make sure that double quotes are not checked
    if LNext2 = L2Quotes then
      LPos := LPos + 2
    else if LNext2 <> L2Quotes then
    begin
      if (LDelim[1] = FQuoteType) and (LQuoteClosed) then
        LQuoteClosed := False
      else if (LDelim[1] = FQuoteType) and (LQuoteClosed = False) then
        LQuoteClosed := True;
      // Do NOT record quote pos if there is still an open quote
      if (LDelim = FFieldSeparator) and (LQuoteClosed) then
      begin
        LDelims.AddDelim(LPos);
        Inc(Result.FieldCount);  // A new delimiter means a new field!
      end;
      LPos := LPos + 1;  // Normally, move forward by 1 char
    end;
    LDelim := '';  // Reset!!!
  end;
  // Try to read in the field if there are no delimiters, i.e only one field
  if LDelims.Count = 0 then
    LFields[0] := pRawRec;
  // Following code until result is returned is for records with more than one field.
  // (Builds the field StringList)
  // There MUST be at least one delimiter
  // There should always be 1 more fields than there are delimiters...
  // Field1,Field2,Field3,Field4,Field5,Field6,Field7 - 6 Delimiters
  // Get the first field (0 to 1st field start)
  if LDelims.Count > 0 then
    LFields[0] := Copy(pRawRec, 1, (LDelims.Delimiters[0].FDelimStart - 1));
  // Get other fields if there is there more than one delimiter
  if LDelims.Count > 1 then
  begin
    // Start from 1 char outside the end of the first delimiter
    for LDelimNo := 0 to (LDelims.Count - 2) do // Stop at the second last delimiter
    begin
      LFieldLen := (LDelims.Delimiters[LDelimNo+1].FDelimStart - 1) - (LDelims.Delimiters[LDelimNo].DelimEnd);
      LFields[LDelimNo + 1] := Copy(pRawRec, (LDelims.Delimiters[LDelimNo].DelimEnd + 1),
        LFieldLen);
    end;
  end;
  // Get the LAST field - first check that there is at least 1 delimiter
  // At this point there should be one field left over for the taking
  if LDelims.Count > 0 then
  begin
    // Work out length of new field
    LFieldLen := LRawRecLen - (LDelims.Delimiters[LDelims.Count-1].DelimEnd);
    LFields[LDelims.Count] := Copy(pRawRec,
      (LDelims.Delimiters[LDelims.Count-1].DelimEnd + 1), LFieldLen);
  end;
  // Limit the number of fields in the DEMO version
  if DemoVersion then
    Result.FieldCount := DemoMaxFields;
  Result.Strings := LFields;
  LDelims.Free;
end;

// Function to remove enclosing double quotes or single quotes from a TStringList
function TODDBImporter.StripQuotes(pQuoteType: Char; pMadeRec: TString256; pFieldCount: Integer): TString256;
var
  LTempList : TString256;
  LField : Integer;
  LToStrip : PChar;

begin
  for LField := 0 to (pFieldCount - 1) do
  begin
    // What I need to do here is change all double quotes to a single quote -
    // except if there is is double quote at the first two chars or a double quote
    // at the last two chars
    LToStrip := PChar(pMadeRec[LField]);
    // Don't bother if first AND last char are not quotes
    if (LToStrip[0] = pQuoteType) and (LToStrip[StrLen(LToStrip) - 1] = pQuoteType) then
      LTempList[LField] := AnsiExtractQuotedStr(LToStrip, pQuoteType)
    else
      LTempList[LField] := pMadeRec[LField]; // Back to where you came from!!!
  end;
  Result := LTempList;
end;

// Function to read the first record after opening the file
// Almost identical to Next, except returns false if RecNo <> 0
function TODDBImporter.First: Boolean;
begin
  if (FOpen = True) and (RecNo = 0) and (Self.Next = True) then
    Result := True
  else Result := False;
end;

// Function to Import the next line from the Import text file, convert it into a
// String list, and optionally, remove enclosing quotes from each field.
// This function will not remove quotes if FQuoteType is equal to FFieldSeparator
// Returns false if unable to advance to the next line, true if all OK
// If an exception occurs, FCurImportRec might not be updated.  This means that
// the user calling this function MUST check that it executed sucessfully before
// getting a field value.
function TODDBImporter.Next: Boolean;
var
  LTempList: TStringRecord;
  LTempStr: String;
  LCommentFound, LineIsRecord: Boolean;

  // This function reads in a new line and checks if it is a comment line
  function CheckForComment(Line: String): Boolean;
  var
    LScan: String;

  begin
    // Get the first couple of chars (as many as the length of the comment string)
    // and compare it to the comment string.
    LScan := Copy(Line, 0, FCommentLen);
    // If a match is not then a comment has been found so skip the current
    // line and move to the next line.  Else all is OK so continue as normal.
    if LScan = FCommentString then
      Result := True
    else Result := False;
  end; { Function CheckForComment }

begin
  // Make sure that the import process has started, if not, then auto start it! (disabled)
  //  if FImportFAssigned and not FOpen then Open;
  Result := False;
  if FFileAssigned and FOpen then
  try
    // Limit the number of records in the DEMO version
    {$IFDEF Demo}
      if FRecNo > DemoMaxRecords then
      begin
        // Check if the Max Number of Records reached for Demo Version Reached
        // has already been shown.
        if not FDemoMRWShown then
        begin
         EODComponentError.Create('This is a DEMO version - Max number of records ('
          + IntToStr(DemoMaxRecords) + ') has been reached!', Self);
         FDemoMRWShown := True;
        end;
        Inc(FRecNo);  // Let user know next rec has been reached, to let EOF work.
        Exit;
      end;
    {$ENDIF}
    // Get the next record from the text file as long as not at end
    //if (FRecNo <= FRecCount) then
    if (FRecNo < FRecCount) then
    begin
      if FAllowComments then
      begin
        ReadLn(FCImpFile, LTempStr);
        Inc(FLineNo);
        LCommentFound := CheckForComment(LTempStr);
        // Check if the line is a valid record. (i.e. not blank)
        LineIsRecord := CheckIfRecord(LTempStr);
        // Loop while the line is (not a record) or a comment
        while (LineIsRecord = False) or LCommentFound do
        begin
          // Get a new line
          ReadLn(FCImpFile, LTempStr);
          Inc(FLineNo);
          // Check if current line is still a comment
          LCommentFound := CheckForComment(LTempStr);
          // Check if the line is still an invalid record. (i.e. blank)
          LineIsRecord := CheckIfRecord(LTempStr);
          // Check if at the EOF
          if System.Eof(FCImpFile) then
          begin
            // The end of the file has been reached
            // The GetEOF function relies on the RecordNo being greater
            // then the record count in order to return TRUE
            Inc(FRecNo);
            Exit;
          end;
        end;
      end
      else begin
        // No checking will be performed for comments.
        ReadLn(FCImpFile, LTempStr);
        Inc(FLineNo);
        // Check if the line is a valid record. (i.e. not blank)
        LineIsRecord := CheckIfRecord(LTempStr);
        // Loop while the line is blank
        while LineIsRecord = False do
        begin
          ReadLn(FCImpFile, LTempStr);
          Inc(FLineNo);
          LineIsRecord := CheckIfRecord(LTempStr);
          // Check if at the EOF
          if System.Eof(FCImpFile) then
          begin
            // The end of the file has been reached
            // The GetEOF function relies on the RecordNo being greater
            // then the record count in order to return TRUE
            Inc(FRecNo);
            Exit;
          end;
        end;
      end;
    end
    else
    begin
      // The end of the file has been reached.  The GetEOF function relies on
      // the RecordNo being greater then the record count in order to return TRUE
      Inc(FRecNo);  // Let the madness end
      Exit;
    end;
    // Make the entire line available to the user
    FOrigRec := LTempStr;
    // Convert raw line to proper record
    if FImportType = itDelimited then // Delimited Import
    begin
      LTempList := MakeRecord(LTempStr);
      // Strip quotes if required
      if FQuoteType <> FFieldSeparator then
        LTempList.Strings := StripQuotes(FQuoteType, LTempList.Strings, LTempList.FieldCount);
    end
    else if FImportType = itFixed then // Fixed Width Import
      LTempList := MakeRecordFixed(LTempStr);
    // Update current record
    FCurImportRec := LTempList;
    // Check if the number of fields matches the header record
    if (FCurImportRec.FieldCount = FHeaderFNRec.FieldCount) and
       (imFieldNames in FHeadersUsed) then
      FRecValid := True
    else if not (imFieldNames in FHeadersUsed) then
      FRecValid := True  // Also valid if there is no FieldNames header record
    else
    begin
      FRecValid := False;
      // Try to log the exception
      if FExceptionLogging = True then
        FExceptionFile.AppendException(FOrigRec, 'Bad_Field_Count', 'N/A', 'N/A', FRecNo);
      // Raise an exception if required
      if FRaiseExceptions then
      // Make the record count one based or zero based???
        raise EODComponentError.Create('Record [' + IntToStr(FRecNo + 1) + '] has a bad field count', Self);
    end;
    Inc(FRecNo);  // Let user know the next record has been reached
    Result := True;
  except
    EODComponentError.Create('Could not read Next record from import file "' +
      FImportFName + '"', Self);
    raise;
  end
end;

function TODDBImporter.NextHeader: TStringRecord;
var
  LTempStr, LScan: String;
  CommentFound: Boolean;

begin
  Result.FieldCount := 0;
  // Skip comments if this feature is enabled (Comments are allowed)
  if FAllowComments then
  begin
    // Check the "first" line.  Only move forward if it is a comment.
    CommentFound := True;
    // Loop while the current line is a comment
    while CommentFound = True do
    begin
      ReadLn(FCImpFile, LTempStr);
      Inc(FLineNo);
      // Get the first couple of chars (as many as the length of the comment string)
      // and compare it to the comment string.
      LScan := Copy(LTempStr, 0, FCommentLen);
      // If a match is not then a comment has been found so skip the current
      // line and move to the next line.  Else all is OK so continue as normal.
      if LScan = FCommentString then
        CommentFound := True
      else CommentFound := False;
    end;
  end;
  if FFileAssigned and FOpen then // Make sure importing is initialised
    if not FUseCtrlFile then
    try
      // Get the next record from the text file as long as not at end
      // Also, do not move forward if the comment scanning code was used
      // as we are already at the next line because of it.
      if (FRecNo <= FRecCount) and (FAllowComments = False) then
      begin
        ReadLn(FCImpFile, LTempStr);
        Inc(FLineNo);
        // Remember, the the (File Cursor) has now moved to the next record!!!
        Result := MakeRecord(LTempStr);
      end
      else if (FRecNo <= FRecCount) and (FAllowComments = True) then
        // Record already read in...
        Result := MakeRecord(LTempStr)
      else Exit;
    except
      Exit;
    end
    else if FUseCtrlFile then
    try
      Result := MakeRecord(FCtrlFile.ReadLine);
    except
      Exit;
    end;
end;

//
// The functions for TODDBImporter's properties
//

function TODDBImporter.GetRecordCount: Integer;
begin
  // Only valid (non-blank) lines are considered records
  // If a line is a comment (and comments are allowed), then it is excluded
  // from the record count.
  // A much faster line scan is achieved if they are not allowed
  Result := GetTFNumLines(FImportFName, FAllowComments, True);
end;

//
// The procedures for the component's properties
//
procedure TODDBImporter.SetImportFName(Value: TFileName);
begin
  // Make sure that the file is 100% valid and readable
  if FileExists(Value) then
  begin
    FImportFName := Value;
    //Close(  // Make sure FCImpFile gets closed
    FFileAssigned := True;
    // Update the record count
    FRecCount := GetRecordCount;
    FOpen := False;
  end
  else if not FileExists(Value) then
  begin
    FFileAssigned := False;
    FImportFName := Value;
  end;
end;

procedure TODDBImporter.SetCDateFormat(Value: String);
begin
  // Make sure that the value entered into the object inspector is a valid
  // A valid date mask format must have at least 3 chars (dmy)
  if Length(Value) >= 3 then
    FCDateFormat := Value
  else raise EODComponentError.Create('A date format must have at least 3 characters!', Self);
end;

procedure TODDBImporter.SetIQryText(pValue: TStringList);
begin
  // ReadOnly!
  //FImportQryText.Assign(pValue);
end;

// Exception Logging Object INTERFACE - These functions and procedures simply
// provide object inspector access to the private FExceptionObject
function TODDBImporter.GetExceptionFName: TFileName;
begin
  Result := FExceptionFile.FileName;
end;

function TODDBImporter.GetExceptionLogging: Boolean;
begin
  Result := FExceptionLogging;
end;

function TODDBImporter.GetExceptionLogMode: TFileAccessMode;
begin
  Result := FExceptionFile.ReqAccessMode;
end;

procedure TODDBImporter.SetExceptionFName(Value: TFileName);
begin
  FExceptionFile.FileName := Value;
end;

procedure TODDBImporter.SetExceptionLogging(Value: Boolean);
begin
  FExceptionLogging := Value;
end;

procedure TODDBImporter.SetExceptionLogMode(Value: TFileAccessMode);
begin
  if (Value = amAppend) or (Value = amReWrite) then
    FExceptionFile.ReqAccessMode := Value;
end;

// Functions to return a String based on a passed index from a TString256

function TODDBImporter.GetHeaderFN(Index: Integer): String;
begin
  if CheckArrIndex(FHeaderFNRec.Strings, Index) = True then
    Result := FHeaderFNRec.Strings[Index]
  else raise Exception.CreateFmt('Field with Index (%d) requested from HeaderFNRec is out of bounds! (Range 0..255)', [Index]);
end;

procedure TODDBImporter.SetHeaderFN(Index: Integer; Value: String);
begin
  if CheckArrIndex(FHeaderFNRec.Strings, Index) = True then
    FHeaderFNRec.Strings[Index] := Value
  else raise Exception.CreateFmt('Field with Index (%d) passed to HeaderFNRec is out of bounds! (Range 0..255)', [Index]);
end;

procedure TODDBImporter.SetCtrlFName(Value: TFileName);
begin
  FCtrlFileName := Value;
  FCtrlFile.FileName := FCtrlFileName;
end;

function TODDBImporter.GetCtrlFName: TFileName;
begin
  Result := FCtrlFileName;
end;

function TODDBImporter.GetRecField(Index: Integer): String;
begin
  if (Index <= HeaderFNCount) and (Index > 1) then
    Result := FCurImportRec.Strings[Index]
  else begin
    EFieldReturnError.Create(IntToStr(Index), Self);
  end;
end;

// Function to return a TODField based on the field index
function TODDBImporter.FieldByNumber(FieldNo: Integer): TODField;
var
  LField : TODField;

begin
  // Create a field with the default date format
  if (FieldNo <= FCurImportRec.FieldCount) and (FieldNo > -1) then
  begin
    LField := TODField.Create(FRecNo, FieldNo, FCDateFormat);
    LField.FieldName := FHeaderFNRec.Strings[FieldNo];
    LField.RaiseExceptions := FRaiseExceptions;
    // Set exception logging options
    if FExceptionLogging then
    begin
      LField.DBEF := FExceptionFile;
      LField.OrigRec := FOrigRec;
    end;
    LField.AsString := FCurImportRec.Strings[FieldNo];
    Result := LField;
  end
  else begin
    // Try to log the exception
    if FExceptionLogging = True then
      FExceptionFile.AppendException(FOrigRec, 'FieldByNumber_Out_Of_Range', IntToStr(FieldNo), 'N/A', FRecNo);
    raise EFieldReturnError.Create(IntToStr(FieldNo), Self);
  end;
end;

function TODDBImporter.FieldByName(FieldName: String): TODField;
var
  LField : TODField;
  LFNo : Integer;

begin
  // Locate the field index by searching through the field array
  LFNo := FindStr(FHeaderFNRec.Strings, FieldName, False);
  if LFNo > -1 then
  begin
    // Create a field with the default date format
    LField := TODField.Create(FRecNo, LFNo, FCDateFormat);
    LField.RaiseExceptions := FRaiseExceptions;
    // Set exception logging options
    if FExceptionLogging then
    begin
      LField.DBEF := FExceptionFile;
      LField.OrigRec := FOrigRec;
    end;
    LField.FieldName := FHeaderFNRec.Strings[LFNo];
    LField.AsString := FCurImportRec.Strings[LFNo];
    Result := LField;
  end
  else
  begin
    // Try to log the exception
    if FExceptionLogging = True then
      FExceptionFile.AppendException(FOrigRec, 'Non-Existent_Field', FieldName, 'N/A', FRecNo);
    raise EFieldReturnError.Create(FieldName, Self);
  end;
end;

function TODDBImporter.FieldByNameTrimmed(FieldName: String): TODField;
begin
  // Return the contents of a field with it's name trimmed of
  // whitespace and control characters.
  Result := FieldByName(Trim(FieldName));
end;

function TODDBImporter.GetAbout: String;
begin
  Result := 'Version ' + ODVersion;
end;

procedure TODDBImporter.SetAbout(Value: String);
begin
  {do nothing}
end;

procedure TODDBImporter.SetCommentString(Value: String);
begin
  // If the comment string is less than 2 chars then disable FAllowComments
  if Length(Value) < 2 then
    FAllowComments := False;
  // Set it anyway.
  FCommentString := Value;
  FCommentLen := Length(Value);
end;

initialization
  Fmt := TFormatSettings.Create;
end.
