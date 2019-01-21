unit ODDBFind;
{
  TODFindMenu Component
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
  VCL.Menus, DB, VCL.StdCtrls, VCL.Buttons, VCL.ExtCtrls, VCL.DBCtrls, VCL.DBGrids;

type
  TODDataLink = class(TDataLink)
  private
    FOnActiveChange: TNotifyEvent;
    function GetDataSet: TDataSet;
  protected
    procedure ActiveChanged; override;
  public
    property DataSet: TDataSet read GetDataSet;
    property OnActiveChange: TNotifyEvent read FOnActiveChange write FOnActiveChange;
  end;

  TODFindForm = class(TForm)
    Bevel1: TBevel;
    FieldCombo: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    ValueEdit: TEdit;
    procedure FormShow(Sender: TObject);
    procedure FieldComboChange(Sender: TObject);
  end;

  TODFindMenu = class(TPopupMenu)
  private
    FUseCount: Integer;
    FDataLink: TODDataLink;
    FDataField: string;
    FFindForm: TODFindForm;
    FFindItem, FFindNextItem: TMenuItem;
    FFindCaption, FFindNextCaption: string;
    FFindShortCut, FFindNextShortCut: TShortCut;
    FAutoField, FAutoSource, FShowPrev, FFindNext: Boolean;
    FOptions: TLocateOptions;
    FOnFind, FOnFindNext: TNotifyEvent;
    function GetAbout: string;
    procedure SetAbout(Value: string);
    function GetDataSource: TDataSource;
    procedure SetDataSource(Value: TDataSource);
    procedure SetDataField(Value: string);
    procedure SetFindShortCut(Value: TShortCut);
    procedure SetFindNextShortCut(Value: TShortCut);
  protected
    procedure DoUpdate(Sender: TObject);
//    procedure DoFind(Sender: TObject);
//    procedure DoFindNext(Sender: TObject);
    procedure UpdateFields;
    function FindRecord(FromStart: Boolean): Boolean;
  public
    procedure DoFind(Sender: TObject);
    procedure DoFindNext(Sender: TObject);    
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property About: string read GetAbout write SetAbout stored False;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DataField: string read FDataField write SetDataField;
    property FindCaption: string read FFindCaption write FFindCaption;
    property FindNextCaption: string read FFindNextCaption write FFindNextCaption;
    property FindShortCut: TShortCut read FFindShortCut write SetFindShortCut;
    property FindNextShortCut: TShortCut read FFindNextShortCut write SetFindNextShortCut;
    property Options: TLocateOptions read FOptions write FOptions default [loCaseInsensitive, loPartialKey];
    property AutoField: Boolean read FAutoField write FAutoField default True;
    property AutoSource: Boolean read FAutoSource write FAutoSource default False;
    property ShowPrev: Boolean read FShowPrev write FShowPrev default False;
    property OnFind: TNotifyEvent read FOnFind write FOnFind;
    property OnFindNext: TNotifyEvent read FOnFindNext write FOnFindNext;
  end;

var
  ODFindForm: TODFindForm;

implementation

{$R *.DFM}

const
  ODVersion = '5.0.0';
  DemoVersion = False;
  HintTrigger = 5;

//TODDataLink

procedure TODDataLink.ActiveChanged;
begin
  inherited ActiveChanged;
  if Assigned(FOnActiveChange) then
    FOnActiveChange(Self);
end;

function TODDataLink.GetDataSet: TDataSet;
begin
  if DataSource <> nil then
    Result := DataSource.DataSet
  else
    Result := nil;
end;

//TODFindForm

procedure TODFindForm.FormShow(Sender: TObject);
begin
  OKBtn.Enabled := FieldCombo.ItemIndex > -1;
  if OKBtn.Enabled then
    ValueEdit.SetFocus
  else
    FieldCombo.SetFocus;
end;

procedure TODFindForm.FieldComboChange(Sender: TObject);
begin
  OKBtn.Enabled := FieldCombo.ItemIndex > -1;
  ValueEdit.Text := '';
  ValueEdit.SetFocus;
end;

//TODFindMenu

constructor TODFindMenu.Create(AOwner: TComponent);
  function InsertItem(ACaption: string; AHandler: TNotifyEvent): TMenuItem;
  begin
    Result := TMenuItem.Create(Self);
    Result.Caption := ACaption;
    Result.OnClick := AHandler;
    Items.Insert(0, Result);
  end;
begin
  inherited Create(AOwner);
  FUseCount := 0;
  FDataLink := TODDataLink.Create;
//  FDataLink.Control := Self;
  FDataLink.OnActiveChange := DoUpdate;
  FFindCaption := '&Find...';
  FFindNextCaption := 'Find &Next';
  FAutoField := True;
  FOptions := [loCaseInsensitive, loPartialKey];
  if not (csDesigning in ComponentState) then
  begin
    FFindForm := TODFindForm.Create(Self);
    if Items.Count > 0 then InsertItem('-', nil);
    FFindNextItem := InsertItem(FFindNextCaption, DoFindNext);
    FFindNextItem.ShortCut := FFindNextShortCut;
    FFindItem := InsertItem(FFindCaption, DoFind);
    FFindItem.ShortCut := FFindShortCut;
  end;
//  SetEditMenu(FEditMenu);
end;

destructor TODFindMenu.Destroy;
begin
  FDataLink.Free;
  inherited Destroy;
end;

procedure TODFindMenu.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FDataLink.DataSource) then
    SetDataSource(nil);
end;

procedure TODFindMenu.DoUpdate(Sender: TObject);
begin
  if csDesigning in ComponentState then Exit;
  FFindItem.Enabled :=
    (FDataLink.DataSet <> nil) and FDataLink.DataSet.Active;
  if not FFindItem.Enabled then
    FFindNextItem.Enabled := False;
  UpdateFields;
end;

procedure TODFindMenu.UpdateFields;
var
  ix: Integer;
begin
//  if csDesigning in ComponentState then Exit;
  FFindForm.FieldCombo.Items.Clear;
  if FDataLink.DataSet <> nil then
    with FDataLink.DataSet do
      for ix := 0 to FieldCount-1 do
        with Fields[ix] do
          if Visible then
            FFindForm.FieldCombo.Items.AddObject(DisplayLabel, Pointer(Index));
end;

procedure TODFindMenu.DoFind(Sender: TObject);
var
  st: string;
  df: TField;
  ix: Integer;
begin
  if DemoVersion then
  begin
    Inc(FUseCount);
    if FUseCount = HintTrigger then
    begin
      MessageDlg('Orbital Decisions FindMenu Component - Unregistered version',
        mtInformation, [mbOK], 0);
      FUseCount := 0;
    end;
  end;
  if FFindForm = nil then
    FFindForm := TODFindForm.Create(Application);
  if FFindForm.FieldCombo.Items.Count = 0 then
    UpdateFields;
  if not (csDesigning in ComponentState) then
  begin
    FFindNextItem.Enabled := False;
    if PopupComponent = nil then  //likely invoked by Popup or shortcut key
      if Owner is TForm then
        PopupComponent := TForm(Owner).ActiveControl;
    if FAutoSource then
      if PopupComponent is TDBEdit then
        DataSource := TDBEdit(PopupComponent).DataSource
      else if PopupComponent is TDBComboBox then
        DataSource := TDBComboBox(PopupComponent).DataSource
      else if PopupComponent is TDBListBox then
        DataSource := TDBListBox(PopupComponent).DataSource
      else if PopupComponent is TDBLookupComboBox then
        DataSource := TDBLookupComboBox(PopupComponent).DataSource
      else if PopupComponent is TDBLookupListBox then
        DataSource := TDBLookupListBox(PopupComponent).DataSource
      else if PopupComponent is TDBGrid then
        DataSource := TDBGrid(PopupComponent).DataSource;
    if (DataField = '') and FAutoField then
    begin
      if PopupComponent is TDBEdit then
        st := TDBEdit(PopupComponent).DataField
      else if PopupComponent is TDBComboBox then
        st := TDBComboBox(PopupComponent).DataField
      else if PopupComponent is TDBListBox then
        st := TDBListBox(PopupComponent).DataField
      else if PopupComponent is TDBLookupComboBox then
        st := TDBLookupComboBox(PopupComponent).DataField
      else if PopupComponent is TDBLookupListBox then
        st := TDBLookupListBox(PopupComponent).DataField
      else if (PopupComponent is TDBGrid) and
              (TDBGrid(PopupComponent).SelectedField <> nil) then
        st := TDBGrid(PopupComponent).SelectedField.FieldName;
    end
    else st := DataField;
    df := FDataLink.DataSet.FindField(st);
    with FFindForm.FieldCombo do
      if df <> nil then
        ItemIndex := Items.IndexOf(df.DisplayLabel)
      else
        ItemIndex := -1;
  end;
  if not FShowPrev then
    FFindForm.ValueEdit.Text := '';
  if FFindForm.ShowModal = ID_OK then
  begin
    ix := Longint(FFindForm.FieldCombo.Items.Objects[
      FFindForm.FieldCombo.ItemIndex]);
    if DataSource.DataSet.Fields[ix].FieldKind = fkData then
      FFindNext := FDataLink.DataSet.Locate(
        DataSource.DataSet.Fields[ix].FieldName,
        FFindForm.ValueEdit.Text, FOptions)
    else
      FFindNext := FindRecord(True);  //do sequential search on calc or lookup fields
    if not (csDesigning in ComponentState) then
    begin
      FFindNextItem.Enabled := FFindNext;
      if Assigned(FOnFind) then FOnFind(Self);
    end;
    if not FFindNext then ShowMessage('No matches found.');
  end;
end;

procedure TODFindMenu.DoFindNext(Sender: TObject);
begin
  if not FFindNext then Exit;
  FFindNext := FindRecord(False);
  if not FFindNext then
    ShowMessage('No more matches found.');
  if not (csDesigning in ComponentState) then
  begin
    FFindNextItem.Enabled := FFindNext;
    if Assigned(FOnFindNext) then FOnFindNext(Self);
  end;
end;

function TODFindMenu.FindRecord(FromStart: Boolean): Boolean;
var
  bm: TBookmark;
  s1, s2: string;
begin
  Result := False;
  with FDataLink.DataSet do
  begin
    bm := GetBookmark;
    Screen.Cursor := crHourglass;
    DisableControls;
    try
      if FromStart then First else Next;
      while not EOF do
      begin
        s1 := FFindForm.ValueEdit.Text;
        with FFindForm.FieldCombo do
          s2 := FDataLink.DataSet.Fields[Longint(Items.Objects[ItemIndex])].AsString;
        if loCaseInsensitive in FOptions then
        begin
          s1 := UpperCase(s1);
          s2 := UpperCase(s2);
        end;
        if loPartialKey in FOptions then
          Result := Pos(s1, s2) = 1
        else
          Result := s1 = s2;
        if Result then Break else Next;
      end;
      if not Result then GotoBookmark(bm);
    finally
      EnableControls;
      Screen.Cursor := crDefault;
      FreeBookmark(bm);
    end;
  end;
end;

function TODFindMenu.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TODFindMenu.SetDataSource(Value: TDataSource);
begin
  FDatalink.DataSource := Value;
  if not (csDesigning in ComponentState) then
  begin
    FFindItem.Enabled := (Value <> nil) and
      (Value.DataSet <> nil) and Value.DataSet.Active;
    FFindNextItem.Enabled := False;
  end;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure TODFindMenu.SetDataField(Value: string);
begin
  if FDataField <> Value then
  begin
    FDataField := Value;
    if not (csDesigning in ComponentState) then
    begin
      FFindNextItem.Enabled := False;
      FFindForm.ValueEdit.Text := '';
    end;
  end;
end;

procedure TODFindMenu.SetFindShortCut(Value: TShortCut);
begin
  FFindShortCut := Value;
  if FFindItem <> nil then
    FFindItem.ShortCut := Value;
end;

procedure TODFindMenu.SetFindNextShortCut(Value: TShortCut);
begin
  FFindNextShortCut := Value;
  if FFindNextItem <> nil then
    FFindNextItem.ShortCut := Value;
end;

{
procedure TODFindMenu.SetEditMenu(Value: Boolean);
  function InsertItem(ACaption: string; AHandler: TNotifyEvent): TMenuItem;
  begin
    Result := TMenuItem.Create(Self);
    Result.Caption := ACaption;
    Result.OnClick := AHandler;
    Items.Insert(0, Result);
  end;
begin
  FDefaultMenu := Value;
  if FDefaultMenu then
  begin
    InsertItem(
    InsertItem('-', nil);
  end;
end;
}
function TODFindMenu.GetAbout: string;
begin
  Result := 'Version ' + ODVersion;
  if DemoVersion then
    Result := Result + ' demo';
end;

procedure TODFindMenu.SetAbout(Value: string);
begin
  {do nothing}
end;

//TODFindMenuEditor


end.
