{
  My Power Station Technology (Pty) Ltd - was Orbital Decisions
  P.O.Box 1080, Milnerton 7435, South Africa
  components@mypowerstation.biz
  http://www.orbital.co.za/text/prodlist.htm
  Copyright (c) 1998-2019

  Use at your own risk!
}

unit ODDBCal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.Menus, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, Data.DB, Vcl.DBCtrls,
  ODCalend, ODPopCal;

type
  TODCustomDBCalendar = class(TODCustomCalendar)
  private
    FStartDateLink, FFinishDateLink: TFieldDataLink;
    FBrowsing, FEditing: Boolean;
    function GetDataSource: TDataSource;
    procedure SetDataSource(Value: TDataSource);
    function GetStartDateName: string;
    procedure SetStartDateName(const Value: string);
    function GetFinishDateName: string;
    procedure SetFinishDateName(const Value: string);
    function GetStartDateField: TField;
    function GetFinishDateField: TField;
  protected
    procedure DoChange; override;
    procedure StartDataChange(Sender: TObject);
    procedure FinishDataChange(Sender: TObject);
    procedure UpdateStartData(Sender: TObject);
    procedure UpdateFinishData(Sender: TObject);
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property StartDateName: string read GetStartDateName write SetStartDateName;
    property FinishDateName: string read GetFinishDateName write SetFinishDateName;
    property StartDateField: TField read GetStartDateField;
    property FinishDateField: TField read GetFinishDateField;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  end;

  TODDBCalendar = class(TODCustomDBCalendar)
  private
    function GetStartDate: TDateTime;
    function GetFinishDate: TDateTime;
  public
    property StartDate read GetStartDate;
    property FinishDate read GetFinishDate;
    property DisplayDate;
    property StartDateField;
    property FinishDateField;
  published
    property DataSource;
    property StartDateName;
    property FinishDateName;
//  property DisplayYear;
//  property DisplayMonth;
//  property DisplayWeek;
    property DayColor;
    property TODayColor;
    property RangeColor;
    property WeekColor;
    property TitleFont;
    property StartFont;
    property FinishFont;
    property PrevYearGlyph;
    property NextYearGlyph;
    property PrevMonthGlyph;
    property NextMonthGlyph;
    property DateFormat;
//  property Framed;
    property MonthNames;
    property Headers;
//  property SingleDate;
    property AutoPage;
    property Plain;
    property ShowStatus;
    property ShowWeeks;
    property ShowYearBtns;
    property StartOnMonday;
//    property TimeEditMask;
//    property TimeEditWidth;
    property UseTime;
    property Align;
    property BevelEdge;
    property Enabled;
    property Color;
//  property Ctl3D;
    property Font;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnDayDblClick;
    property OnStartClick;
    property OnFinishClick;
    property OnSelectYear;
    property OnSetupDay;
    property OnEnter;
    property OnExit;
    property OnResize;
  end;

  TODDBPopupCalendar = class(TODPopupCalendar)
  private
    FStartDateLink, FFinishDateLink: TFieldDataLink;
    FEditing: Boolean;
    function GetDataSource: TDataSource;
    procedure SetDataSource(Value: TDataSource);
    function GetStartDateName: string;
    procedure SetStartDateName(const Value: string);
    function GetFinishDateName: string;
    procedure SetFinishDateName(const Value: string);
    function GetStartDateField: TField;
    function GetFinishDateField: TField;
    function GetStartDate: TDateTime;
    function GetFinishDate: TDateTime;
  protected
    procedure ButtonClick(Sender: TObject); override;
    procedure Change(Sender: TObject); override;
    procedure StartDataChange(Sender: TObject);
    procedure FinishDataChange(Sender: TObject);
    procedure UpdateStartData(Sender: TObject);
    procedure UpdateFinishData(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property StartDate read GetStartDate;
    property FinishDate read GetFinishDate;
    property StartField: TField read GetStartDateField;
    property FinishField: TField read GetFinishDateField;
  published
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property StartDateName: string read GetStartDateName write SetStartDateName;
    property FinishDateName: string read GetFinishDateName write SetFinishDateName;
  end;

implementation

constructor TODCustomDBCalendar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStartDateLink := TFieldDataLink.Create;
  FStartDateLink.Control := Self;
  FStartDateLink.OnDataChange := StartDataChange;
  FStartDateLink.OnUpdateData := UpdateStartData;
  FFinishDateLink := TFieldDataLink.Create;
  FFinishDateLink.Control := Self;
  FFinishDateLink.OnDataChange := FinishDataChange;
  FFinishDateLink.OnUpdateData := UpdateFinishData;
  FBrowsing := True;
end;

destructor TODCustomDBCalendar.Destroy;
begin
  FStartDateLink.Free;
  FFinishDateLink.Free;
  FStartDateLink := nil;
  FFinishDateLink := nil;
  inherited Destroy;
end;

procedure TODCustomDBCalendar.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then
  begin
    StartDataChange(Self);
    FinishDataChange(Self);
  end;
end;

procedure TODCustomDBCalendar.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FStartDateLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TODCustomDBCalendar.GetDataSource: TDataSource;
begin
  Result := FStartDateLink.DataSource;
end;

procedure TODCustomDBCalendar.SetDataSource(Value: TDataSource);
begin
  FStartDateLink.DataSource := Value;
  FFinishDateLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TODCustomDBCalendar.GetStartDateName: string;
begin
  Result := FStartDateLink.FieldName;
end;

procedure TODCustomDBCalendar.SetStartDateName(const Value: string);
begin
  FStartDateLink.FieldName := Value;
  SingleDate := FFinishDateLink.FieldName = '';
end;

function TODCustomDBCalendar.GetFinishDateName: string;
begin
  Result := FFinishDateLink.FieldName;
end;

procedure TODCustomDBCalendar.SetFinishDateName(const Value: string);
begin
  FFinishDateLink.FieldName := Value;
  SingleDate := Value = '';
end;

function TODCustomDBCalendar.GetStartDateField: TField;
begin
  Result := FStartDateLink.Field;
end;

function TODCustomDBCalendar.GetFinishDateField: TField;
begin
  Result := FFinishDateLink.Field;
end;

procedure TODCustomDBCalendar.DoChange;
begin
  if not FBrowsing then       //if not moving to another record
  begin
    FEditing := True;
    FStartDateLink.Edit;
    FFinishDateLink.Edit;
    FStartDateLink.Modified;
    FFinishDateLink.Modified;
    FStartDateLink.UpdateRecord;
    FFinishDateLink.UpdateRecord;
    FEditing := False;
  end;
  inherited DoChange;
end;

procedure TODCustomDBCalendar.StartDataChange(Sender: TObject);
var
  yr, mo, dy: Word;
begin
  if FEditing then Exit;
  FBrowsing := True;
  try
    if FStartDateLink.Field = nil then
      StartDate := 0
    else
    begin
      StartDate := FStartDateLink.Field.AsDateTime;
      if StartDate <> 0 then
      begin
        DecodeDate(StartDate, yr, mo, dy);
        DisplayYear := yr;
        DisplayMonth := mo;
      end;
    end;
    if SingleDate then
      FinishDate := StartDate;
  finally
    FBrowsing := False;
  end;
end;

procedure TODCustomDBCalendar.FinishDataChange(Sender: TObject);
begin
  if FEditing then Exit;
  FBrowsing := True;
  try
    if FFinishDateLink.Field = nil then
      FinishDate := StartDate
    else
      FinishDate := FFinishDateLink.Field.AsDateTime;
  finally
    FBrowsing := False;
  end;
end;

procedure TODCustomDBCalendar.UpdateStartData(Sender: TObject);
begin
  if FStartDateLink.Field <> nil then
    FStartDateLink.Field.AsDateTime := StartDate;
end;

procedure TODCustomDBCalendar.UpdateFinishData(Sender: TObject);
begin
  if FFinishDateLink.Field <> nil then
    FFinishDateLink.Field.AsDateTime := FinishDate;
end;

//TODDBCalendar

function TODDBCalendar.GetStartDate: TDateTime;
begin
  Result := inherited StartDate;
end;

function TODDBCalendar.GetFinishDate: TDateTime;
begin
  Result := inherited FinishDate;
end;

//TODDBPopupCalendar

constructor TODDBPopupCalendar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStartDateLink := TFieldDataLink.Create;
  FStartDateLink.Control := Self;
  FStartDateLink.OnDataChange := StartDataChange;
  FStartDateLink.OnUpdateData := UpdateStartData;
  FFinishDateLink := TFieldDataLink.Create;
  FFinishDateLink.Control := Self;
  FFinishDateLink.OnDataChange := FinishDataChange;
  FFinishDateLink.OnUpdateData := UpdateFinishData;
  StoreDates := False;
end;

destructor TODDBPopupCalendar.Destroy;
begin
  FStartDateLink.Free;
  FFinishDateLink.Free;
  inherited Destroy;
end;

procedure TODDBPopupCalendar.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then
  begin
    StartDataChange(Self);
    FinishDataChange(Self);
  end;
end;

procedure TODDBPopupCalendar.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FStartDateLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

function TODDBPopupCalendar.GetDataSource: TDataSource;
begin
  Result := FStartDateLink.DataSource;
end;

procedure TODDBPopupCalendar.SetDataSource(Value: TDataSource);
begin
  FStartDateLink.DataSource := Value;
  FFinishDateLink.DataSource := Value;
end;

function TODDBPopupCalendar.GetStartDateName: string;
begin
  Result := FStartDateLink.FieldName;
end;

procedure TODDBPopupCalendar.SetStartDateName(const Value: string);
begin
  FStartDateLink.FieldName := Value;
  SingleDate := FFinishDateLink.FieldName = '';
end;

function TODDBPopupCalendar.GetFinishDateName: string;
begin
  Result := FFinishDateLink.FieldName;
end;

procedure TODDBPopupCalendar.SetFinishDateName(const Value: string);
begin
  FFinishDateLink.FieldName := Value;
  SingleDate := Value = '';
end;

function TODDBPopupCalendar.GetStartDateField: TField;
begin
  Result := FStartDateLink.Field;
end;

function TODDBPopupCalendar.GetFinishDateField: TField;
begin
  Result := FFinishDateLink.Field;
end;

procedure TODDBPopupCalendar.ButtonClick(Sender: TObject);
begin
  if csDesigning in ComponentState then
  begin
    FButton.Down := False;
    Exit;
  end;
  if (DataSource = nil) or (DataSource.DataSet = nil) or
      not DataSource.DataSet.Active or (StartDateName = '') then
  begin
    ShowMessage('Date data not fully defined or inactive.');
    FButton.Down := False;
  end else
  begin
    FEditing := True;
    inherited ButtonClick(Sender);
    if FForm.ModalResult = mrOK then
      Change(Self);
    FEditing := False;
  end;
end;

procedure TODDBPopupCalendar.Change(Sender: TObject);
begin
  inherited Change(Sender);
  if not (csDesigning in ComponentState) then
  begin
    FEditing := True;
    FStartDateLink.Edit;
    FFinishDateLink.Edit;
    FStartDateLink.Modified;
    FFinishDateLink.Modified;
    FStartDateLink.UpdateRecord;
    FFinishDateLink.UpdateRecord;
    FEditing := False;
  end;
end;

procedure TODDBPopupCalendar.StartDataChange(Sender: TObject);
begin
  if FEditing then Exit;
  if (FStartDateLink.Field = nil) or not FStartDateLink.Active then
    StartDate := 0
  else
  begin
    StartDate := FStartDateLink.Field.AsDateTime;
    if StartDate <> 0 then
      DisplayDate := StartDate;
  end;
  if SingleDate then
    FinishDate := StartDate;
end;

procedure TODDBPopupCalendar.FinishDataChange(Sender: TObject);
begin
  if FEditing then Exit;
  if FFinishDateLink.Field <> nil then
    FinishDate := FFinishDateLink.Field.AsDateTime;
end;

procedure TODDBPopupCalendar.UpdateStartData(Sender: TObject);
begin
  if FStartDateLink.Field <> nil then
    FStartDateLink.Field.AsDateTime := StartDate;
end;

procedure TODDBPopupCalendar.UpdateFinishData(Sender: TObject);
begin
  if FFinishDateLink.Field <> nil then
    FFinishDateLink.Field.AsDateTime := FinishDate;
end;

function TODDBPopupCalendar.GetStartDate: TDateTime;
begin
  Result := inherited StartDate;
end;

function TODDBPopupCalendar.GetFinishDate: TDateTime;
begin
  Result := inherited FinishDate;
end;

end.
