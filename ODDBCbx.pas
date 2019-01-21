unit ODDBCbx;
{
  TODDBComboBox Component
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
  VCL.Menus, VCL.StdCtrls, VCL.Buttons, VCL.ExtCtrls, Data.DB, FireDAC.Comp.Client, VCL.DBCtrls;

type
  TODDBComboBox = class(TCustomComboBox)
  private
    FDataLink: TFieldDataLink;
    FAutoUpdate: Boolean;
    function GetAbout: string;
    procedure SetAbout(Value: string);
    function GetDataSource: TDataSource;
    procedure SetDataSource(Value: TDataSource);
    function GetDataField: string;
    procedure SetDataField(const Value: string);
    function GetField: TField;
    function GetItems: TStrings;
  protected
    procedure ActiveChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    property Field: TField read GetField;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure UpdateList;
    property Items: TStrings read GetItems;
    property Text;
  published
    property About: string read GetAbout write SetAbout stored False;
    property AutoUpdate: Boolean read FAutoUpdate write FAutoUpdate default True;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DataField: string read GetDataField write SetDataField;
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

implementation


const
  ODVersion = '5.0.0';
  DemoVersion = False;
  MaxItems = 8;

constructor TODDBComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnActiveChange := ActiveChange;
  FDataLink.OnEditingChange := EditingChange;
  FAutoUpdate := True;
//  Style := csDropDownList;
end;

destructor TODDBComboBox.Destroy;
begin
  FDataLink.Free;
  inherited Destroy;
end;

procedure TODDBComboBox.Loaded;
begin
  inherited Loaded;
  UpdateList;
end;

procedure TODDBComboBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TODDBComboBox.UpdateList;
var
  Bmk: TBookmark;
  ix: Integer;
begin
  Items.Clear;
  if (csDesigning in ComponentState) or (csLoading in ComponentState) or
    (FDataLink.Field = nil) or not FDataLink.Active then Exit;
  with FDataLink.DataSet do
  begin
    Bmk := GetBookmark;
    DisableControls;
    try
      First;
      ix := 1;
      while not EOF do
      begin
        Items.Add(FDataLink.Field.AsString);
        Next;
        Inc(ix);
        if (ix > MaxItems) and DemoVersion then
          Break;  //only allow MaxItems in demo version
      end;
      GotoBookmark(Bmk);
    finally
      EnableControls;
      FreeBookmark(Bmk);
    end;
  end;
end;

function TODDBComboBox.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TODDBComboBox.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;

function TODDBComboBox.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TODDBComboBox.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

function TODDBComboBox.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TODDBComboBox.GetItems: TStrings;
begin
  Result := inherited Items;
end;

procedure TODDBComboBox.ActiveChange(Sender: TObject);
begin
  UpdateList;
end;

procedure TODDBComboBox.EditingChange(Sender: TObject);
begin
  if FAutoUpdate and (FDataLink.DataSet.State = dsBrowse) then
    UpdateList;
end;

function TODDBComboBox.GetAbout: string;
begin
  Result := 'Version ' + ODVersion;
  if DemoVersion then
    Result := Result + ' demo';
end;

procedure TODDBComboBox.SetAbout(Value: string);
begin
  {do nothing}
end;

end.
