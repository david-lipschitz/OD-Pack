unit ODDBExporter;

{ TODDBExporter - By Alan Givati - For Orbital Descisions South Africa
  Copyright (c) 1998-2019
  Orbital Decisions:- My Power Station Technology (Pty) Ltd
  P.O.Box 1080, Milnerton 7435, South Africa
  components@mypowerstation.biz
  http://www.orbital.co.za/text/prodlist.htm

  Version 1.4.2 - Compatible With Delphi XE8
  Version 5.0.0 - Compatible With Delphi 10.3 Rio

  Use at your own risk!

  ( + = Todo, - = Possible Issues, # = Just Done.  Now do documentation & checking )
  + Use TField's visible flag to decide whether or not to export that field.

  Format:

  pValue -  prefix 'p' added to all procedure and function parameters
  LVar -  prefix 'l' added to all variables local to a procedure or function
  FVar - prefiex 'F' added to all private class variables

  The Words "Delimiter" and "Field Separator" are used interchangably!
    SO DONT GET CONFUSED!!!

  Component to easily manage the saving of an SQL result to a text file.
  This file may or may not be delimited.   In both cases, double quotes
  may be used to separate the values from each other (if required).

  ***EXPORTING***

  Can export to a plain Delimited file or Fixed Format as in the import examples.
  Headers may be written to the ExportFile or to a separate ControlFile (but
  not both).

  Other export types:
  -------------------
  Note:  This IS what the exported file could look like.
         FieldLength may be Zero if not a String/VarChar/Char
         Zero means that the field length is the default size (e.g. 4Bytes for Integer)

  Delimited Format Export File:
  ==================================================================
  TestColumn1,TestColumn2,TestColumn3,TestColumn4,TestColumn5NEWLINE
  12,23.435,AlanG was here,15:25,12/03/2000NEWLINE
  730,582.03,"May the force be with you",07:00,30/23/2000NEWLINE
  ==================================================================

  Equivalent Fixed Format Export File:
  ==================================================================
  TestColumn1,TestColumn2,TestColumn3,TestColumn4,TestColumn5NEWLINE
  3,5,26,5,10
  123.435  "Someone was here"         15:2512/03/2000NEWLINE
  730582.03"May the force be with you"07:0030/23/2000NEWLINE
  ==================================================================
}

// Include the DCR file with the components icon... Not Required for D4+
{ $INCLUDE ODDBExporter.dcr }

interface

{$INCLUDE ODDatXSet.pas}

uses
  {Windows, Messages, }System.SysUtils, System.Classes{, Graphics, Controls},
  Vcl.Forms, Vcl.Dialogs, Data.DB,
  {FileCtrl,} FireDAC.Comp.Client,
  {$IFDEF Full} ODDBCommon;
  {$ELSE} ODDBCommon, ODDemo; {$ENDIF}

type
  // Types of field reformatting - ftBasic is just the field separator
  // ftStrict is all ASCII characters 0 - 32 including blanks
  // ftAlways encloses all fields in quotes regardless of contents
  TReFmtType = (ftAlways, ftStrict, ftBasic);

  { This code was shared by ODDBCommon }
  { Types of header records - Field Names, Field Types and Field Lengths }
  THeaderType = (exFieldNames, exFieldTypes, exFieldLengths);
  THeaderTypes = set of THeaderType;

  // These message handlers handle events / progress notification from the
  // Export() function
  TDBProgressEvent = procedure (Sender: TObject; Total, RecordNo: Integer) of object;
  // ProcName is the procedure / function namne whih raised the event
  TDBAbortEvent = procedure (Sender: TObject; ProcName: String) of object;


  TODDBExporter = class(TFDQuery)
  private
    { Private declarations }
    { Non-Object members }
    // Stores type of quotes that are used by Export() when formatting the output
    // records.  Again, see FmtFieldData's internal comments for more info
    FQuoteType: Char;
    FStopRunning: Boolean;        // Set to True if Demo or Trial Version caught running outside Delphi
    FTrialExpired: Boolean;       // Set to True if Trial version should die...
    FFieldSeparator: String;      // Character/s used to separate fields in a record
    FAllowReFmt: Boolean;         // May fields be reformatted for safety purposes???
    FEndCurrentProcess: Boolean;  // Should the current job (exporting) be killed???
    FReFmtType: TReFmtType;       // Reformatting type (ftStrict, ftBasic)
    FMarkNulls: Boolean;          // Store null values as NULL
    FExpFile: TODTextFile;        // The Export Textfile
    FCtrlFile: TODTextFile;       // Used to store headers if selected
    FLeftPadChar: Char;           // Used to pad numeric fields in fixed format exports
    FRightPadChar: Char;          // Used for non-numeric field padding in fixed format exports
    FExportHeaders: THeaderTypes; // Which headers should be generated
    FUseCtrlFile: Boolean;        // Should a control file be used to store headers?
    FDemoMRWShown: Boolean;
    // EVENTS
    // Event to notify of export progress
    FOnProgress: TDBProgressEvent;
    // Event for the cancelation of an export
    FOnAbort: TDBAbortEvent;
    { General Functions }
    // Function to check and correct a field of a record about to be imported or exported
    function FmtFieldData(pFieldData: String; pReFmtType: TReFmtType): String;
    { General Procedures }
    procedure WriteHeaders(pFieldCount: Integer; TextObj: TODTextFile);
    { Functions for properties  }
    function GetAbout: string;
    function GetExportFName: TFileName;
    function GetExpFAMode: TFileAccessMode;
    function GetCtrlFName: TFileName;
    { Procedures for properties }
    procedure SetAbout(Value: string);
    procedure SetExportFName(Value: TFileName);
    procedure SetCtrlFName(Value: TFileName);
    procedure SetExpFAMode(Value: TFileAccessMode);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ExportDelimited;
    procedure ExportFixed;
  published
    { Published declarations }
    property About: string read GetAbout write SetAbout stored False;
    property ControlFile : TFileName read GetCtrlFName write SetCtrlFName;
    property ExportFile : TFileName read GetExportFName write SetExportFName;
    property ExportMode : TFileAccessMode read GetExpFAMode write SetExpFAMode default amReWrite;
    property ExportQuoteType : Char read FQuoteType write FQuoteType;
    property FieldSeparator : String read FFieldSeparator write FFieldSeparator;
    property ReFormat : Boolean read FAllowReFmt write FAllowReFmt default False;
    property ReFormatType : TReFmtType read FReFmtType write FReFmtType default ftBasic;
    property MarkNulls : Boolean read FMarkNulls write FMarkNulls default False;
    property EndCurrentTask : Boolean read FEndCurrentProcess write FEndCurrentProcess;
    property LeftPadChar : Char read FLeftPadChar write FLeftPadChar;
    property RightPadChar : Char read FRightPadChar write FRightPadChar;
    property HeadersToExport : THeaderTypes read FExportHeaders write FExportHeaders;
    property UseControlFile : Boolean read FUseCtrlFile write FUseCtrlFile;
    // Events
    property OnProgress: TDBProgressEvent read FOnProgress write FOnProgress;
    property OnAbort: TDBAbortEvent read FOnAbort write FOnAbort;
  end;

implementation


//
// TODDBExporter Implementation
//

constructor TODDBExporter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDemoMRWShown := False;
  FFieldSeparator := ',';
  FQuoteType := '"';
  FRightPadChar := ' ';
  FLeftPadChar := '0';
  FAllowReFmt := True;    // Most likely case (I Hope).  Maybe some people like problems?
  // Most common reason to use "" is if delimiter character inside the field's value
  FReFmtType := ftBasic;
  FMarkNulls := False;     // This seems to the be more common pre-requisite
  FStopRunning := False;
  FTrialExpired := False;
  FExpFile := TODTextFile.Create(Self);
  FCtrlFile := TODTextFile.Create(Self);
  // Do demo-version/trial version checking (Both versions my only run with Delphi)
  {$IFDEF Full}
  {$ELSE}
    if (DemoVersion = True) and (DelphiLoaded = False) then
      FStopRunning := True;
    if (TrialVersion = True) and (DelphiLoaded = False) then
      FStopRunning := True;
  {$ENDIF}
  {$IFDEF Trial}
    if CheckTrial(Self) = True then
      FTrialExpired := True;
  {$ENDIF}
end;

destructor TODDBExporter.Destroy;
begin
  // Get rid of everything that shouldn't be!
  FExpFile.Free;
  FCtrlFile.Free;
  inherited Destroy;
end;

//
// General Functions
//

function TODDBExporter.FmtFieldData(pFieldData: String; pReFmtType: TReFmtType): String;
var
  LPos, LNewLength, LCheckAscii : Integer;
  LError : Boolean;
  LNewField : String;

begin
  // This function makes sure that the supplied data string is correct, and if
  // not, corrects it to comply with requirements.  This means that, if any
  // potentially problematic characters are encountered, that the string becomes
  // enclosed in double quotes.
  LError := False;
  LNewLength := Length(pFieldData);
  // Check if the passed value is a null one.
  // If it is and markNulls is on, then return 'NULL' else just Return nothing
  if (LNewLength = 0) and (FMarkNulls = True) then
  begin
    Result := 'NULL';
    Exit;
  end
  else if (LNewLength = 0) and (FMarkNulls = False) then
  begin
    Result := '';
    Exit;
  end;
  for LPos := 0 to LNewLength - 1 do
    for LCheckAscii := 1 to 32 do
      // Check if the current character is ASCII 1 through 32 (32 is a whitespace)
      case pReFmtType of
      ftAlways:
        LError := True;
      ftStrict:
        if (pFieldData[LPos] = Chr(LCheckAscii)) or (pFieldData[LPos] = FFieldSeparator) then
          LError := True;
      ftBasic:
        if pFieldData[LPos] = FFieldSeparator then
          LError := True;
      end;
  if LError then
  begin
    // Enclose the string in user specified quotes
    LNewField := AnsiQuotedStr(pFieldData, FQuoteType);
    Result := LNewField;
  end
  else
    Result := pFieldData;
end;

// Proc to write all requested headers to current export file (presumed OPEN)
procedure TODDBExporter.WriteHeaders(pFieldCount: Integer; TextObj: TODTextFile);
var
  LCurrField : Integer;
  LFieldHeader : String;

begin
  // Build the delimited FieldName list if requested
  if exFieldNames in FExportHeaders then
  begin
    LFieldHeader := '';
    for LCurrField := 0 to (pFieldCount - 1) do
    begin
      LFieldHeader := LFieldHeader + Fields[LCurrField].FieldName;
      // Add a comma if not at the LAST field
      if LCurrField < (FieldDefs.Count - 1) then
        LFieldHeader := LFieldHeader + FFieldSeparator;
    end;
//    try
      TextObj.WriteLine(LFieldHeader);
//    except
//      ShowMessage('Problem writing table field names to ' + TextObj.FileName);
//    end;
  end;
  // Build the delimited FieldTypes list if requested
  if exFieldTypes in FExportHeaders then
  begin
    LFieldHeader := '';
    for LCurrField := 0 to (pFieldCount - 1) do
    begin
      LFieldHeader := LFieldHeader + GetDataTypeAsString(Fields[LCurrField].DataType);
      // Add a comma if not at the LAST field
      if LCurrField < (FieldDefs.Count - 1) then
        LFieldHeader := LFieldHeader + FFieldSeparator;
    end;
    try
      TextObj.WriteLine(LFieldHeader);
    except
      ShowMessage('Problem writing table field types to ' + TextObj.FileName);
    end;
  end;
  // Build the delimited FieldLengths list if requested
  if exFieldLengths in FExportHeaders then
  begin
    LFieldHeader := '';
    for LCurrField := 0 to (pFieldCount - 1) do
    begin
      LFieldHeader := LFieldHeader + IntToStr(Fields[LCurrField].DisplayWidth);
      // Add a comma if not at the LAST field
      if LCurrField < (FieldDefs.Count - 1) then
        LFieldHeader := LFieldHeader + FFieldSeparator;
    end;
    try
      TextObj.WriteLine(LFieldHeader);
    except
      ShowMessage('Problem writing table field lengths to ' + TextObj.FileName);
    end;
  end;
end;

procedure TODDBExporter.ExportDelimited;
var
  LCurrField, LCurrRec : Integer;
  LOutRec : String;
  LRecCount, LFieldCount : Integer;

begin
  LOutRec := '';
  // Exports all data once the query has been executed (provided it is valid)
  FEndCurrentProcess := False;  // Would be quite stupid if nothing ever happened!!!
  {$IFDEF Full}
    // Do nothing
  {$ELSE}
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
  // Note:  AccessMode is only available once the ODTextFile is Open
  if FExpFile.Open and (Active = True) and (FExpFile.AccessMode <> amReadOnly) and
    (FExpFile.AccessMode <> amNone) then
  try
    FDemoMRWShown := False;
    // Go through all the records - but make sure at first record
    First;
    LRecCount := RecordCount - 1;
    LFieldCount := FieldCount - 1;
    // Limit the number of fields in the DEMO version
    if DemoVersion and (FieldCount > DemoMaxFields) then
    begin
      EODComponentError.Create('This is a DEMO version - The maximum number of '+
        'Fields per Record is 5!', Self);
      Exit;  // Jumps straight to finally block
    end;
    // Export the table headers
    if FUseCtrlFile then
    begin
      if FCtrlFile.Open then
        WriteHeaders(FieldCount, FCtrlFile);
      FCtrlFile.Close;
    end
    else WriteHeaders(FieldCount, FExpFile);  // Use export file if not ctrl file
    for LCurrRec := 0 to (LRecCount) do
    begin
      if FEndCurrentProcess then
      begin
        // Raise an FOnAbort Event to allow for external handling...
        if Assigned(OnAbort) then
          FOnAbort(Self, 'Delimited Export');
        Abort;  // Skip straight to the finally block
      end;
      for LCurrField := 0 to LFieldCount do // And all the fields
      begin
        // Build up the record string to write - first check if reformatting enabled
        if FAllowReFmt = true then
          LOutRec := LOutRec + FmtFieldData(Fields[LCurrField].AsString, FReFmtType)
        else
          LOutRec := LOutRec + Fields[LCurrField].AsString;
        // Add a comma if not at the LAST field
        if LCurrField < (FieldDefs.Count - 1) then
          LOutRec := LOutRec + FFieldSeparator;
      end;
      // Write the current record to file
      try
        FExpFile.WriteLine(LOutRec);
      except
        ShowMessage('Problem writing record No. ' + IntToStr(LCurrRec) + ' to ' + FExpFile.FileName);
      end;
      // Limit the number of records in the DEMO version
      if DemoVersion and (LCurrRec > DemoMaxRecords) then
      begin
        // Check if the "Max Number of Records reached for Demo Version Reached"
        // msg has already been shown.
        if not FDemoMRWShown then
        begin
          EODComponentError.Create('This is a DEMO version - Max number of records ('
           + IntToStr(DemoMaxRecords) + ') has been reached!', Self);
          FDemoMRWShown := True;
        end;
        Exit;
      end;
      // Increment the current record and reset the LOutRec string
      Next;
      LOutRec := '';  // Reset!
      Application.ProcessMessages; // Allow to be terminated
      // Let the world know of the current export progress
      if Assigned(OnProgress) then
        FOnProgress(Self, LRecCount, LCurrRec);
    end;
  finally
    FExpFile.Close;
  end
  else if FExpFile.AccessMode = amReadOnly then
    Showmessage('Could not export table - the filename was read-only!')
  else if FExpFile.AccessMode = amNone then
    Showmessage('Could not export table - the filename was invalid!')
  else if Active = False then
    Showmessage('Could not export table - query invalid!');
end;

// Procedure to export result as a fixed format file
// Uses the TFields and their DisplayWidth properties to set the number of chars
// per field, and DisplayLabel to specify the character to use for padding
procedure TODDBExporter.ExportFixed;
var
  LCurrField, LCurrRec : Integer;
  LOutRec, LNewField : String;
  LRecCount, LFieldCount, LWDiff, LPad : Integer;

begin
  LOutRec := '';
  LNewField := '';
  // Do not bother writing out header
  // Exports all data once the query has been executed (provided it is valid)
  FEndCurrentProcess := False;  // Would be quite stupid if nothing ever happened!!!
  {$IFDEF Full}
    // Do nothing
  {$ELSE}
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
  if FExpFile.Open and (Active = True) and (FExpFile.AccessMode <> amNone) and
    (FExpFile.AccessMode <> amReadOnly) then
  try
    FDemoMRWShown := False;
    LRecCount := RecordCount - 1;
    LFieldCount := FieldCount - 1;
    // Make sure at first record
    Self.First;
    // Limit the number of fields in the DEMO version
    if DemoVersion and (FieldCount > DemoMaxFields) then
    begin
      EODComponentError.Create('This is a DEMO version - The maximum number of '+
        'Fields per Record is 5!', Self);
      Exit;
    end;
    // Export the table headers
    if FUseCtrlFile then
    begin
      // The control file is ALWAYS overwritten/re-created
      FCtrlFile.ReqAccessMode := amReWrite;
      if FCtrlFile.Open then
      begin
        WriteHeaders(FieldCount, FCtrlFile);
        FCtrlFile.Close;
      end
      else raise EODComponentError.Create('function ExportFixed - Unable to write headers to control file.', Self);
    end
    else WriteHeaders(FieldCount, FExpFile);  // Use export file if not ctrl file
    for LCurrRec := 0 to (LRecCount) do
    begin
      if FEndCurrentProcess then
      begin
        // Raise an FOnAbort Event to allow for external handling...
        if Assigned(OnAbort) then
          FOnAbort(Self, 'Fixed Format Export');
        Abort;  // Skip straight to the finally block
      end;
      for LCurrField := 0 to (LFieldCount) do // And all the fields
      begin
        // Build up the record string to write - use specified widths
        LNewField := Copy(Fields[LCurrField].AsString, 1, Fields[LCurrField].DisplayWidth);
        // Check if the new field is smaller than the desired width - If so pad it
        LWDiff := Fields[LCurrField].DisplayWidth - Length(LNewField);
        // Check what type of field - if it is a string, it gets right-padded
        if (Fields[LCurrField] is TStringField) then
        begin
          if LWDiff > 0 then
            for LPad := 1 to LWDiff do
              LNewField := LNewField + FRightPadChar;
        end
        // If the field is numeric format, left pad it with zeros
        else if (Fields[LCurrField] is TNumericField) or (Fields[LCurrField] is TDateTimeField) then
        begin
          if LWDiff > 0 then
            for LPad := 1 to LWDiff do
              LNewField := FLeftPadChar + LNewField;
        end;
        LOutRec := LOutRec + LNewField;
      end;
      // Write the current record to file
      try
        FExpFile.WriteLine(LOutRec);
      except
        ShowMessage('Problem writing record No. ' + IntToStr(LCurrRec) + ' to ' + FExpFile.FileName);
      end;
      // Limit the number of records in the DEMO version
      if DemoVersion and (LCurrRec > DemoMaxRecords) then
      begin
        // Check if the "Max Number of Records reached for Demo Version Reached"
        // msg has already been shown.
        if not FDemoMRWShown then
        begin
          EODComponentError.Create('This is a DEMO version - Max number of records ('
           + IntToStr(DemoMaxRecords) + ') has been reached!', Self);
          FDemoMRWShown := True;
        end;
        Exit;
      end;
      // Increment the current record and reset the LOutRec string
      Next;
      LOutRec := '';  // Reset!
      Application.ProcessMessages; // Allow to be terminated
      // Let the world know of the current export progress
      if Assigned(OnProgress) then
        FOnProgress(Self, LRecCount, LCurrRec);
    end;
  finally
    FExpFile.Close;
  end
  else if FExpFile.AccessMode = amReadOnly then
    ShowMessage('Could not export table - the filename was read-only!')
  else if FExpFile.AccessMode = amNone then
    ShowMessage('Could not export table - the filename was invalid!')
  else if Active = False then
    ShowMessage('Could not export table - query invalid!');
end;

//
// The procedures for the component's properties
//

procedure TODDBExporter.SetExportFName(Value: TFileName);
begin
  FExpFile.FileName := Value;
end;

procedure TODDBExporter.SetExpFAMode(Value: TFileAccessMode);
begin
  // FileName must exist in order to append to it!!!
  if FileExists(FExpFile.FileName) = False then
  begin
    if (Value = amRewrite) or (Value = amNone) then
      FExpFile.ReqAccessMode := Value
    else if Value = amReadOnly then
      raise EODComponentError.Create('Cannot read from a non-existent file!', Self)
    else if Value = amAppend then
      raise EODComponentError.Create('Cannot append to a non-existent file!', Self);
  end
  else if FileExists(FExpFile.FileName) = True then
  begin
    if (FExpFile.Attributes.ReadOnly) and (Value <> amReadOnly) and (Value <> amNone) then
    begin
      FExpFile.ReqAccessMode := amReadOnly;
      raise EODComponentError.Create('Cannot write to a read-only file!', Self);
    end
    else FExpFile.ReqAccessMode := Value;
  end;
end;

function TODDBExporter.GetExportFName: TFileName;
begin
  Result := FExpFile.FileName
end;

function TODDBExporter.GetCtrlFName: TFileName;
begin
  Result := FCtrlFile.FileName;
end;

function TODDBExporter.GetExpFAMode: TFileAccessMode;
begin
  Result := FExpFile.ReqAccessMode;;
end;

procedure TODDBExporter.SetCtrlFName(Value: TFileName);
begin
  FCtrlFile.FileName := Value;
end;

function TODDBExporter.GetAbout: string;
begin
  Result := 'Version ' + ODVersion;
end;

procedure TODDBExporter.SetAbout(Value: string);
begin
  {do nothing}
end;

end.
