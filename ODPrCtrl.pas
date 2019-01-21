unit ODPrCtrl;
{
  TODPriceController Component
  My Power Station Technology (Pty) Ltd - was Orbital Decisions
  P.O.Box 1080, Milnerton 7435, South Africa
  components@mypowerstation.biz
  http://www.orbital.co.za/text/prodlist.htm
  Copyright (c) 1998-2019

  Use at your own risk!
}

interface

uses
  Windows, Messages, SysUtils, Classes, VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs,
  VCL.Buttons, VCL.StdCtrls, VCL.Mask, VCL.DBCtrls, VCL.Grids, VCL.DBGrids, VCL.ExtCtrls,
  Data.DB, FireDAC.Comp.Client, FireDAC.Comp.DataSet, FireDAC.Stan.Error,
  FireDAC.DatS, FireDAC.Stan.Option, FireDAC.Stan.Intf;

type
  TODVerificationErrorEvent = procedure(Sender: TObject;
    const FieldValues: string) of object;
{ TODbValidateUpdateEvent = procedure(Sender: TObject; var AFromDate: TDateTime;
    var APrice: Extended; var Accept: Boolean) of object;}

  TODPriceController = class;

  TODPriceFieldNames = class(TPersistent)
  private
    FKeyFields, FFromDate, FToDate, FPrice: string;
  protected
  public
    PriceController: TODPriceController;
    constructor Create;

  published
    property KeyFields: string read FKeyFields write FKeyFields;
    property FromDate: string read FFromDate write FFromDate;
    property ToDate: string read FToDate write FToDate;
    property Price: string read FPrice write FPrice;
  end;

  TODPriceController = class(TComponent)
  private
    FItemTableName, FPriceTableName: string;
    FPriceFieldNames: TODPriceFieldNames;
    FUpdateQuery: TFDQuery;
    FItemDataSet: TDataSet;
    FTerminalDate: TDateTime;
    FVerifyUpdate, FVerifyAll: Boolean;
    FOnBeforeUpdate, FOnAfterUpdate, FOnUpdateError: TNotifyEvent;
    FOnCachedUpdateError: TFDUpdateErrorEvent;
    FOnValidateUpdate: {TODValidateUpdateEvent}TNotifyEvent;
    FOnVerificationError: TODVerificationErrorEvent;
    FDateTimeFormatSettings: TFormatSettings;
    function GetAbout: string;
    procedure SetAbout(Value: string);
    function GetConnectionName: string;
    procedure SetConnectionName(Value: string);
    procedure SetItemDataSet(Value: TDataSet);
    procedure SetTerminalDate(Value: TDateTime);
    function GetCachedUpdates: Boolean;
    procedure SetCachedUpdates(Value: Boolean);
    procedure GetKeyFields(var FieldList: TStrings);
    function KeyFilterStr: string;
    procedure CachedUpdateError(ASender: TDataSet; AException: EFDException;
      ARow: TFDDatSRow; ARequest: TFDUpdateRequest;
      var AAction: TFDErrorAction);
    function GetDateTimeFormatSettings: TFormatSettings;
    function SQLiteDateTimeStringToDateTime(
      const SQLiteDateTime: string): TDateTime;
    function DateTimeStringToDateTime(const strDateTime: string): TDateTime;
    procedure ChangeLastDate(RevDate: TDateTime);
  protected
    procedure CheckDataState;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure AddPrice(StartDate: TDateTime; NewPrice: Extended);
    procedure AddBatchPrice(StartDate: TDateTime; NewPrice: Extended;
      const BatchFilter: string);
    procedure VerifyDates;
  published
    property About: string read GetAbout write SetAbout stored False;
    property CachedUpdates: Boolean read GetCachedUpdates write SetCachedUpdates default True;
    property VerifyUpdate: Boolean read FVerifyUpdate write FVerifyUpdate default True;
    property VerifyAll: Boolean read FVerifyAll write FVerifyAll default True;
    property ItemDataSet: TDataSet read FItemDataSet write SetItemDataSet;
    property ConnectionName: string read GetConnectionName write SetConnectionName;
    property ItemTableName: string read FItemTableName write FItemTableName;
    property PriceTableName: string read FPriceTableName write FPriceTableName;
    property PriceFieldNames: TODPriceFieldNames read FPriceFieldNames write FPriceFieldNames;
    property TerminalDate: TDateTime read FTerminalDate write SetTerminalDate;
    property DateFormat: string read FDateTimeFormatSettings.ShortDateFormat write FDateTimeFormatSettings.ShortDateFormat;
    property OnBeforeUpdate: TNotifyEvent read FOnBeforeUpdate write FOnBeforeUpdate;
    property OnAfterUpdate: TNotifyEvent read FOnAfterUpdate write FOnAfterUpdate;
    property OnUpdateError: TNotifyEvent read FOnUpdateError write FOnUpdateError;
    property OnCachedUpdateError: TFDUpdateErrorEvent read FOnCachedUpdateError write FOnCachedUpdateError;
    property OnValidateUpdate: {TODValidateUpdateEvent}TNotifyEvent read FOnValidateUpdate write FOnValidateUpdate;
    property OnVerificationError: TODVerificationErrorEvent read FOnVerificationError write FOnVerificationError;
  end;


  TODPriceCtlDefTable = class(TFDTable)
  public
    procedure InitFieldDefs; override;
//    procedure InitIndexDefs; override;
  end;

implementation

uses System.DateUtils;

const
  ODVersion = '5.0.0';
  ODProductName = 'Price Controller Component';
  RestrictedVersion = False;
  MinPrice = 0;
  MaxPrice = 100;


//class constructor TODPriceController.Create;
//begin
//  inherited;
//  FSqliteFormatSettings := GetSQLiteFormatSettings;
//end;
//
//class function TODPriceController.GetSQLiteFormatSettings: TFormatSettings;
//begin
//  Result:= FormatSettings;
//  Result.DateSeparator:='-';
//  Result.TimeSeparator:=':';
//  Result.ShortDateFormat:='YYYY-MM-DD';
//  Result.ShortTimeFormat:='HH:MM:SS';
//end;
//
//class function TODPriceController.SQLiteDateTimeStringToDateTime(
//  const SQLiteDateTime: string): TDateTime;
//var
//  DTString: string;
//begin
//  DTString := SQLiteDateTime.Replace('T',' ',[rfIgnoreCase]);
//  case Length(DTString) of
//    16: FSQLiteFormatSettings.ShortTimeFormat:='HH:MM';
//    19: FSQLiteFormatSettings.ShortTimeFormat:='HH:MM:SS';
//    23: FSQLiteFormatSettings.ShortTimeFormat:='HH:MM:SS.ZZZ';
//  end;
//  Result := StrToDateTime(DTString, FSQLiteFormatSettings);
//  FSQLiteFormatSettings.ShortTimeFormat:='HH:MM:SS';
//end;
//
function TODPriceController.GetDateTimeFormatSettings: TFormatSettings;
begin
  Result:= FormatSettings;
  Result.DateSeparator:='-';
  Result.TimeSeparator:=':';
  Result.DecimalSeparator:= '.';
  Result.ShortDateFormat:='YYYY-MM-DD';
  Result.ShortTimeFormat:='HH:MM:SS';
end;

function TODPriceController.DateTimeStringToDateTime(
  const strDateTime: string): TDateTime;
begin
  if (Length(strDateTime) > 4) and CharInSet(strDateTime.Chars[4], ['-','T']) then
  begin
    Result := SQLiteDateTimeStringToDateTime(strDateTime);
    Exit;
  end;
  Result := StrToDateTime(strDateTime);
end;

function TODPriceController.SQLiteDateTimeStringToDateTime(
  const SQLiteDateTime: string): TDateTime;
var
  DTString: string;
begin
  DTString := SQLiteDateTime.Replace('T',' ',[rfIgnoreCase]);
  case Length(DTString) of
    16: FDateTimeFormatSettings.ShortTimeFormat:='HH:MM';
    19: FDateTimeFormatSettings.ShortTimeFormat:='HH:MM:SS';
    23: FDateTimeFormatSettings.ShortTimeFormat:='HH:MM:SS.ZZZ';
  end;
  Result := StrToDateTime(DTString, FDateTimeFormatSettings);
  FDateTimeFormatSettings.ShortTimeFormat:='HH:MM:SS';
end;

constructor TODPriceFieldNames.Create;
begin
  inherited Create;
  FFromDate := 'FromDate';
  FToDate := 'ToDate';
  FPrice := 'Price';
end;

constructor TODPriceController.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDateTimeFormatSettings := GetDateTimeFormatSettings;
  FVerifyUpdate := True;
  FVerifyAll := True;
//  FDateFormat := 'yyyy-mm-dd'; //sqlite format date
               //'dd-mmm-yyyy';
  FTerminalDate := EncodeDate(2049,12,31); //StrToDateTime('31/12/2049',FFormatSettings);
  FPriceFieldNames := TODPriceFieldNames.Create;
  FPriceFieldNames.PriceController := Self;
  FUpdateQuery := TFDQuery.Create(Self);
  FUpdateQuery.CachedUpdates := True;
  FUpdateQuery.OnUpdateError := CachedUpdateError;
end;

destructor TODPriceController.Destroy;
begin
  FPriceFieldNames.Free;
  inherited Destroy;
end;

procedure TODPriceController.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FItemDataSet) then
    FItemDataSet := nil;
end;

procedure TODPriceController.CheckDataState;
  procedure DataError(const Msg: string);
  begin
    raise EComponentError.Create(Msg);
  end;
begin
  if ConnectionName = '' then DataError('ConnectionName undefined');
  if FItemDataSet = nil then DataError('ItemDataSet not assigned');
  if not FItemDataSet.Active then DataError('ItemDataset not open');
  if FPriceTableName = '' then DataError('PriceTableName undefined');
  if FPriceFieldNames.KeyFields = '' then DataError('PriceFieldNames.KeyFields undefined');
  if FPriceFieldNames.FromDate = '' then DataError('PriceFieldNames.FromDate undefined');
  if FPriceFieldNames.ToDate = '' then DataError('PriceFieldNames.ToDate undefined');
  if FPriceFieldNames.Price = '' then DataError('PriceFieldNames.Price undefined');
end;

procedure TODPriceController.GetKeyFields(var FieldList: TStrings);
var
  ix: Integer;
  st: string;
begin
  st := '';
  FieldList.Clear;
  for ix := 1 to Length(FPriceFieldNames.KeyFields) do
    if FPriceFieldNames.KeyFields[ix] <> ';' then
      st := st + FPriceFieldNames.KeyFields[ix]
    else
    begin
      FieldList.Add(st);
      st := '';
    end;
  if st <> '' then FieldList.Add(st);
end;

function TODPriceController.KeyFilterStr: string;
var
  sList: TStrings;
  ix: Integer;
  st: string;
begin
  Result := 'WHERE ';
  sList := TStringList.Create;
  try
    GetKeyFields(sList);
    for ix := 0 to sList.Count-1 do
    begin
      st := FItemDataSet.FieldByName(sList[ix]).AsString;
      if not (FItemDataSet.FieldByName(sList[ix]) is TNumericField) then
        st := '"' + st + '"';
      Result := Result + sList[ix] + ' = ' + st;
      if ix < sList.Count-1 then
        Result := Result + ' AND ';
    end;
  finally
    sList.Free;
  end;
end;

//change last date of latest price to day before new price start day
procedure TODPriceController.ChangeLastDate(RevDate: TDateTime);
var
  PrevDate: TDateTime;
begin
  with FUpdateQuery do
  begin
    //get last ToDate value
    SQL.Clear;
    SQL.Add(Format('SELECT MAX(%s) FROM %s',
      [FPriceFieldNames.ToDate, FPriceTableName]));
    SQL.Add(KeyFilterStr);
    try
      Open;
      PrevDate:= DateTimeStringToDateTime(Fields[0].AsString);
//      if PrevDate >= RevDate then
//        raise EComponentError.Create(FPriceFieldNames.ToDate +
//          ' value must be larger than that of the current price');
    finally
      Close;
    end;
    SQL.Clear;
    SQL.Add('UPDATE ' + FPriceTableName);
    SQL.Add(Format('SET %s = "%s"',
      [FPriceFieldNames.ToDate, FormatDateTime(DateFormat, RevDate)]));
    SQL.Add(KeyFilterStr + Format(' AND %s = "%s"',
//      [FPriceFieldNames.ToDate, FormatDateTime(DateFormat, FTerminalDate)]));
      [FPriceFieldNames.ToDate, FormatDateTime(DateFormat, PrevDate)]));
    try
      ExecSQL;
    except
      on Exception do
      begin
        if Assigned(FOnUpdateError) then FOnUpdateError(Self);
        raise;
      end;
    end;
  end;
end;

//add new price record
procedure TODPriceController.AddPrice(StartDate: TDateTime; NewPrice: Extended);
var
  sList: TStrings;
  st, sf: string;
  ix: Integer;
  LDate: TDate;
begin
  if RestrictedVersion and
    ((NewPrice < MinPrice) or (NewPrice > MaxPrice)) then
  begin
    MessageDlg('This evaluation version only accepts prices between ' +
      IntToStr(MinPrice) + '.00 and ' + IntToStr(MaxPrice) + '.00',
      mtInformation, [mbOK], 0);
    Exit;
  end;
  CheckDataState;
  with FUpdateQuery do
  begin
    //check if StartDate > existing max FromDate value
    SQL.Clear;
    SQL.Add(Format('SELECT MAX(%s) FROM %s',
      [FPriceFieldNames.FromDate, FPriceTableName]));
    SQL.Add(KeyFilterStr);
    try
      Open;
      LDate:= DateTimeStringToDateTime(Fields[0].AsString);
      if LDate > StartDate then
        raise EComponentError.Create(FPriceFieldNames.FromDate +
          ' value must be larger than that of the current price');
    finally
      Close;
    end;
    if Assigned(FOnBeforeUpdate) then FOnBeforeUpdate(Self);
    //adjust prev current price
    ChangeLastDate(StartDate - 1);
    //add new current price
    SQL.Clear;
    st := '';
    sList := TStringList.Create;
    try
      GetKeyFields(sList);
      for ix := 0 to sList.Count-1 do
        st := st + sList[ix] + ', ';
      SQL.Add(Format('INSERT INTO %s (%s%s, %s, %s)', [FPriceTableName, st,
        FPriceFieldNames.FromDate, FPriceFieldNames.ToDate, FPriceFieldNames.Price]));
      st := '';
      for ix := 0 to sList.Count-1 do
      begin
        sf := FItemDataSet.FieldByName(sList[ix]).AsString;
        if not (FItemDataSet.FieldByName(sList[ix]) is TNumericField) then
          sf := '"' + sf + '"';
        st := st + sf + ', ';
      end;
    finally
      sList.Free;
    end;
    SQL.Add(Format('VALUES (%s"%s", "%s", %f)',
      [st, FormatDateTime(DateFormat, StartDate),
       FormatDateTime(DateFormat, FTerminalDate), NewPrice], FDateTimeFormatSettings));
    try
      ExecSQL;
    except
      on Exception do
      begin
        if Assigned(FOnUpdateError) then FOnUpdateError(Self);
        raise;
      end;
    end;
    if Assigned(FOnAfterUpdate) then FOnAfterUpdate(Self);
  end;
  if FVerifyUpdate then VerifyDates;
end;

procedure TODPriceController.AddBatchPrice(StartDate: TDateTime;
  NewPrice: Extended; const BatchFilter: string);
var
  sList: TStrings;
  aQuery: TFDQuery;
  ds: TDataSet;
  ix: Integer;
  st: string;
begin
  if FItemTableName = '' then
    raise EComponentError.Create('ItemTableName undefined');
  sList := TStringList.Create;
  aQuery := TFDQuery.Create(Self);
  ds := FItemDataSet;    //temp holder
  FItemDataSet := ds;   //use batch query
  with aQuery do
  try
    Connection := FUpdateQuery.Connection;
    GetKeyFields(sList);
    st := '';
    for ix := 0 to sList.Count-1 do
      if ix < sList.Count-1 then
        st := st + sList[ix] + ', '
      else
        st := st + sList[ix];
    SQL.Add('SELECT DISTINCT ' + st);
    SQL.Add('FROM ' + FItemTableName);
    SQL.Add('WHERE ' + BatchFilter);
    Prepare;
    Open;
    First;
    while not EOF do
    begin
      AddPrice(StartDate, NewPrice);
      Next;
    end;
    Close;
  finally
    FItemDataSet := ds;
    Free;
    sList.Free;
  end;
end;

procedure TODPriceController.VerifyDates;

  procedure CheckDates;

    procedure DoVerificationError;
    var
      sList: TStrings;
      st: string;
      ix: Integer;
    begin
      sList := TStringList.Create;
      with FUpdateQuery, FPriceFieldNames do
      try
        GetKeyFields(sList);
        for ix := 0 to sList.Count-1 do
          st := st + FieldByName(sList[ix]).AsString + ', ';
        OnVerificationError(Self, st + FieldByName(FromDate).AsString +
          ', ' + FieldByName(ToDate).AsString);
      finally
        sList.Free;
      end;
    end;

  var
    PrevFromDate: TDateTime;
  begin//CheckDates
    with FUpdateQuery, FPriceFieldNames do
    begin
    //      if FieldByName(ToDate).AsDateTime <
//          FieldByName(FromDate).AsDateTime then
      if DateTimeStringToDateTime(FieldByName(ToDate).AsString) <
         DateTimeStringToDateTime(FieldByName(FromDate).AsString) then
        if Assigned(FOnVerificationError) then
          DoVerificationError
        else
          raise EComponentError.Create('Price date period error');
//      PrevFromDate := FieldByName(FPriceFieldNames.FromDate).AsDateTime;
      PrevFromDate := DateTimeStringToDateTime(
        FieldByName(FPriceFieldNames.FromDate).AsString);
      Next;
      if FVerifyAll and not EOF then
      begin
//        if FieldByName(ToDate).AsDateTime <> PrevFromDate - 1 then
        if DateTimeStringToDateTime(FieldByName(ToDate).AsString) <>
           PrevFromDate - 1 then
          if Assigned(FOnVerificationError) then
            DoVerificationError
          else
            raise EComponentError.Create('Price date sequence error');
        CheckDates;   //recursively repeat check
      end;
    end;
  end;

begin//VerifyPriceDates
  with FUpdateQuery do
  begin
    if Assigned(FOnValidateUpdate) then
      FOnValidateUpdate(Self)    //custom validation
    else
    begin
      SQL.Clear;
      SQL.Add('SELECT * FROM ' + FPriceTableName);
      SQL.Add(KeyFilterStr);
      SQL.Add('ORDER BY ' + FPriceFieldNames.ToDate + ' DESC');
      Open;
      if RecordCount = 0 then
        if Assigned(FOnVerificationError) then
          OnVerificationError(Self, 'No prices found')
        else
          raise EComponentError.Create('No prices found')
      else
        CheckDates;
      Close;
    end;
  end;
end;

procedure TODPriceController.CachedUpdateError(ASender: TDataSet;
  AException: EFDException; ARow: TFDDatSRow; ARequest: TFDUpdateRequest;
  var AAction: TFDErrorAction);
begin
  if Assigned(FOnCachedUpdateError) then
    FOnCachedUpdateError(ASender, AException, ARow, ARequest, AAction);
end;

function TODPriceController.GetConnectionName: string;
begin
  Result := FUpdateQuery.ConnectionName;
end;

procedure TODPriceController.SetConnectionName(Value: string);
begin
  FUpdateQuery.ConnectionName := Value;
end;

procedure TODPriceController.SetItemDataSet(Value: TDataSet);
begin
  FItemDataSet := Value;
  if Value <> nil then
  begin
    if (FUpdateQuery.ConnectionName = '') and (Value is TFDCustomQuery) then
      FUpdateQuery.ConnectionName := TFDCustomQuery(Value).ConnectionName;
    if (Value is TFDTable) and (FPriceFieldNames.KeyFields = '') then
    begin
      FPriceFieldNames.KeyFields := TFdTable(Value).IndexFieldNames;
      FItemTableName := TFdTable(Value).TableName;
    end;
    Value.FreeNotification(Self);
  end;
end;

procedure TODPriceController.SetTerminalDate(Value: TDateTime);
begin
  if Value <> FTerminalDate then
    if csDesigning in ComponentState then
    begin
      if not (csLoading in ComponentState) then
        MessageDlg('The terminal dates of existing prices ' +
          'will need to be updated.', mtWarning, [mbOK], 0);
    end
    else
      //update the terminal dates of all prices
      if (FPriceTableName <> '') and (FPriceFieldNames.ToDate <> '') then
        with FUpdateQuery do
        begin
          SQL.Clear;
          SQL.Add('UPDATE ' + FPriceTableName);
          SQL.Add(Format('SET %s = "%s"',
            [FPriceFieldNames.ToDate, FormatDateTime(DateFormat, Value)]));
          SQL.Add(Format('WHERE %s = "%s"',
            [FPriceFieldNames.ToDate, FormatDateTime(DateFormat, FTerminalDate)]));
          if Assigned(FOnBeforeUpdate) then FOnBeforeUpdate(Self);
          try
            ExecSQL;
          except
            on Exception do
            begin
              if Assigned(FOnUpdateError) then FOnUpdateError(Self);
              raise;
            end;
          end;
          if Assigned(FOnAfterUpdate) then FOnAfterUpdate(Self);
        end;
  FTerminalDate := Value;
end;

function TODPriceController.GetCachedUpdates: Boolean;
begin
  Result := FUpdateQuery.CachedUpdates;
end;

procedure TODPriceController.SetCachedUpdates(Value: Boolean);
begin
  FUpdateQuery.CachedUpdates := Value;
end;



{ TODPriceCtlDefTable ----------------------------------------}

procedure TODPriceCtlDefTable.InitFieldDefs;
begin
  inherited InitFieldDefs;
end;
{
procedure TODPriceCtlDefTable.InitIndexDefs;
begin
  inherited InitIndexDefs;
end;
}
function TODPriceController.GetAbout: string;
begin
  Result := 'Version ' + ODVersion;
  if RestrictedVersion then
    Result := Result + ' demo';
end;

procedure TODPriceController.SetAbout(Value: string);
begin
  {do nothing}
end;


end.
