unit ODFldCbx;

interface

uses
  Windows, Messages, SysUtils, Classes, VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs,
  VCL.Menus, VCL.StdCtrls, VCL.Buttons, VCL.ExtCtrls, Data.DB, FireDAC.Comp.Client;

type
  TODFieldComboBox = class(TCustomComboBox)
  private
    FFDQuery: TFDQuery;
    FFDConnection: TFDCustomConnection;
    FAutoUpdate, FFiltered: Boolean;
    FConnectionName, FTableName, FFieldName, FFilter: string;
    OwnsFDConnection: Boolean;
    function GetAbout: string;
    procedure SetAbout(Value: string);
    procedure SetConnectionName(const Value: string);
    procedure SetTableName(const Value: string);
    procedure SetFieldName(const Value: string);
    procedure SetFilter(const Value: string);
    procedure SetFiltered(Value: Boolean);
    function GetItems: TStrings;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure UpdateList;
    property Items: TStrings read GetItems;
    property Text;
  published
    property About: string read GetAbout write SetAbout stored False;
    property AutoUpdate: Boolean read FAutoUpdate write FAutoUpdate default True;
    property ConnectionName: string read FConnectionName write SetConnectionName stored True;
    property TableName: string read FTableName write SetTableName stored True;
    property FieldName: string read FFieldName write SetFieldName stored True;
    property Filter: string read FFilter write SetFilter;
    property Filtered: Boolean read FFiltered write SetFiltered default False;
    property Color;
    property Ctl3D;
    property DragMode;
    property DragCursor;
    property DropDownCount;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property Style;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnStartDrag;
  end;


  TODFieldComboBoxDefTable = class(TFDTable)
  public
    procedure InitFieldDefs; override;
  end;

implementation

uses System.UITypes;

const
  ODVersion = '2.1.3';
  DemoVersion = False;
  MaxItems = 8;

constructor TODFieldComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OwnsFDConnection := False;
//  FFDConnection := TFDConnection.Create(AOwner);
  FFDQuery := TFDQuery.Create(Self);
//  FFDQuery.Connection := FFDConnection;
  FAutoUpdate := True;
//  Style := csDropDownList;
end;

destructor TODFieldComboBox.Destroy;
begin
  FFDQuery.Active := False;
  FFDQuery.Free;
//  FFDConnection.Connected := False;
  if OwnsFDConnection then
    FFDConnection.Free;
  inherited Destroy;
end;

procedure TODFieldComboBox.Loaded;
begin
  inherited Loaded;
  if FAutoUpdate then UpdateList;
end;

procedure TODFieldComboBox.UpdateList;
  procedure DoError(const PN: string);
  begin
    raise EComponentError.Create(PN + ' not defined');
  end;
var
  ix: Integer;
  NewConn: TFDCustomConnection;
begin
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
    Exit;
  Items.Clear;
  if FAutoUpdate then
  begin
    if (FConnectionName = '') or (FTableName = '') or (FFieldName = '') then
        Exit
  end
  else
  begin
    if FConnectionName = '' then DoError('ConnectionName');
    if FTableName = '' then DoError('TableName');
    if FFieldName = '' then DoError('FieldName');
  end;
  NewConn := FDManager.FindConnection(ConnectionName);
  if not Assigned(NewConn) then
  begin
    FFDConnection := TFDConnection.Create(Self);
    OwnsFDConnection := True;
    FFDConnection.ConnectionDefName := ConnectionName;
    FFDConnection.Connected := True;
//    MessageDlg('Connection "' + ConnectionName + '" is not found', mtError, [mbOK], 0);
//    ConnectionName := '';
//    Exit;
  end else
  if NewConn <> FFDConnection then
  begin
    if OwnsFDConnection then
    begin
      FFDConnection.Free;
      OwnsFDConnection:= False;
    end;
    FFDConnection := NewConn;
    FFDConnection.Connected := True;
  end;
  with FFDQuery do
  begin
    Close;
    Connection := FFDConnection;
    SQL.Clear;
    SQL.Add('SELECT DISTINCT ' + FFieldName + ' FROM ' + FTableName);
    if FFiltered then
      if FFilter = '' then DoError('Filter')
      else SQL.Add('WHERE ' + FFilter);
    Open;
    First;
    ix := 1;
    while not EOF do
    begin
      Items.Add(Fields[0].AsString);
      Next;
      Inc(ix);
      if (ix > MaxItems) and DemoVersion then
        Break;  //only allow MaxItems in demo version
    end;
    Close;
  end;
end;

procedure TODFieldComboBox.SetConnectionName(const Value: string);
begin
  FConnectionName := Value;
  UpdateList;
end;

procedure TODFieldComboBox.SetTableName(const Value: string);
begin
  FTableName := Value;
  UpdateList;
end;

procedure TODFieldComboBox.SetFieldName(const Value: string);
begin
  FFieldName := Value;
  UpdateList;
end;

procedure TODFieldComboBox.SetFilter(const Value: string);
begin
  FFilter := Value;
  if FFiltered then UpdateList;
end;

procedure TODFieldComboBox.SetFiltered(Value: Boolean);
begin
  FFiltered := Value;
  UpdateList;
end;

function TODFieldComboBox.GetItems: TSTrings;
begin
  Result := inherited Items;
end;

procedure TODFieldComboBoxDefTable.InitFieldDefs;
begin
  inherited InitFieldDefs;
end;

function TODFieldComboBox.GetAbout: string;
begin
  Result := 'Version ' + ODVersion;
  if DemoVersion then
    Result := Result + ' demo';
end;

procedure TODFieldComboBox.SetAbout(Value: string);
begin
  {do nothing}
end;

end.

