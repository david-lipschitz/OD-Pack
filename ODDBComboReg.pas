unit ODDBComboReg;
{
  My Power Station Technology (Pty) Ltd - was Orbital Decisions
  P.O.Box 1080, Milnerton 7435, South Africa
  components@mypowerstation.biz
  http://www.orbital.co.za/text/prodlist.htm
  Copyright (c) 1998-2019

  Use at your own risk!
}

interface

uses
  DesignIntF, DesignEditors, Classes;

type
  TODFieldComboBoxConnectionNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TODFieldComboBoxTableNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TODFieldComboBoxFieldNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

procedure Register;

implementation

uses
  Vcl.Dialogs, FireDAC.Comp.Client, System.SysUtils, ODDBCbx, ODFldCbx;

type
  TODAboutProperty = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

const
  ODVersion = '5.0.0';
  DemoVersion = False;
  MaxItems = 8;

procedure Register;
begin
  RegisterComponents('Orbital', [TODFieldComboBox]);
  RegisterPropertyEditor(TypeInfo(string), TODFieldComboBox,
    'ConnectionName', TODFieldComboBoxConnectionNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TODFieldComboBox,
    'TableName', TODFieldComboBoxTableNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TODFieldComboBox,
    'FieldName', TODFieldComboBoxFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TODFieldComboBox,
    'About', TODAboutProperty);

  RegisterComponents('Orbital', [TODDBComboBox]);
  RegisterPropertyEditor(TypeInfo(string), TODDBComboBox, 'About', TODAboutProperty);

end;

{ TODFieldComboBoxDatabaseNameProperty --------------------------------------}

function TODFieldComboBoxConnectionNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TODFieldComboBoxConnectionNameProperty.GetValues(Proc: TGetStrProc);
var
  st: TStrings;
  ix: Integer;
  ConnName: String;
  LFDConn: TFDCustomConnection;
begin
  st := TStringList.Create;
  try
    FDManager.GetConnectionNames(st);
    for ix:= 0 to FDManager.ConnectionCount-1 do
    begin
      LFDConn := FDManager.Connections[ix];
      if LFDConn.ConnectionName <> '' then
        Continue;       //it is in list already
      ConnName := LFDConn.Name;
      if ConnName = '' then
      begin
        ConnName := 'Unnamed' + ix.ToString;
      end;
      LFDConn.ConnectionName := ConnName;
      st.Add(ConnName);
    end;
    //    Session.GetDatabaseNames(st);
    for ix := 0 to st.Count-1 do
      Proc(st[ix]);
  finally
    st.Free;
  end;
end;

{ TODFieldComboBoxTableNameProperty --------------------------------------}

function TODFieldComboBoxTableNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TODFieldComboBoxTableNameProperty.GetValues(Proc: TGetStrProc);
var
  st: TStrings;
  ix: Integer;
begin
  with GetComponent(0) as TODFieldComboBox do
    if ConnectionName <> '' then
    begin
      st := TStringList.Create;
      try
        FDManager.GetTableNames(ConnectionName, '', '', '', st);
//        Session.GetTableNames(DatabaseName, '*.*',False, False, st);
        for ix := 0 to st.Count-1 do
          Proc(st[ix]);
      finally
        st.Free;
      end;
    end;
end;

{ TODFieldComboBoxFieldNameProperty --------------------------------------}

function TODFieldComboBoxFieldNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TODFieldComboBoxFieldNameProperty.GetValues(Proc: TGetStrProc);
var
  ix: Integer;
//  dt: TODFieldComboBoxDefTable;
  cb: TODFieldComboBox;
  st: TStrings;
begin
  cb := GetComponent(0) as TODFieldComboBox;
  if (cb.ConnectionName <> '') and (cb.TableName <> '') then
  begin
    st := TStringList.Create;
    try
      FDManager.GetFieldNames(cb.ConnectionName, '', '', cb.TableName, '', st);
      for ix := 0 to st.Count-1 do
        Proc(st[ix]);
    finally
      st.Free;
    end;
//    dt := TODFieldComboBoxDefTable.Create(cb);
//    try
//      dt.DatabaseName := cb.DatabaseName;
//      dt.TableName := cb.TableName;
//      dt.InitFieldDefs;
//      for ix := 0 to dt.FieldDefs.Count-1 do
//        Proc(dt.FieldDefs[ix].Name);
//    finally
//      dt.Free;
//    end;
  end;
end;

function TODAboutProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

procedure TODAboutProperty.Edit;
begin
  MessageDlg('Orbital Decisions FieldComboBox Component ' + ODVersion +
    #13#13 + 'By: My Power Station Technology (Pty) Ltd'#13 +
    'Delphi development, solutions and business components'#13#13 +
    'Addr.:   P.O.Box 1080, Milnerton, 7435, South Africa'#13 +
    'EMail:  components@mypowerstation.biz'#13 +
    'URL:    http://www.orbital.co.za',
    mtInformation, [mbOK], 0);
end;
{
procedure TODAboutProperty.Edit;
begin
  MessageDlg('Orbital Decisions DBComboBox Component ' + ODVersion +
    #13#13 + 'Orbital Decisions:- My Power Station Technology (Pty) Ltd'#13 +
    'Delphi development, solutions and business components'#13#13 +
    'Addr.:   P.O.Box 1080, Milnerton, 7435, South Africa'#13 +
    'EMail:  components@mypowerstation.biz'#13 +
    'URL:    http://www.orbital.co.za',
    mtInformation, [mbOK], 0);
end;
}


end.
