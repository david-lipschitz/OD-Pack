unit ODPrCtrlReg;
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
  DesignIntf, DesignEditors, ODPrCtrl, Classes;

type
  TODPriceCtlConnectionNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TODpriceCtlTableNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TODPriceCtlFieldNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TODPriceCtlKeyFieldNamesProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;
procedure Register;

implementation

uses
  {Graphics,Controls,}FireDAC.Comp.Client, Vcl.Dialogs, System.SysUtils;

type
  TODAboutProperty = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

const
  ODVersion = '2.1.3.1';
  ODProductName = 'Price Controller Component';
  RestrictedVersion = False;
  MinPrice = 0;
  MaxPrice = 100;

procedure Register;
begin
  RegisterComponents('Orbital', [TODPriceController]);
  RegisterPropertyEditor(TypeInfo(string), TODPriceController,
    'ConnectionName', TODPriceCtlConnectionNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TODPriceController,
    'ItemTableName', TODPriceCtlTableNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TODPriceController,
    'PriceTableName', TODPriceCtlTableNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TODPriceFieldNames,
    'Price', TODPriceCtlFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TODPriceFieldNames,
    'FromDate', TODPriceCtlFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TODPriceFieldNames,
    'ToDate', TODPriceCtlFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TODPriceFieldNames,
    'KeyFields', TODPriceCtlKeyFieldNamesProperty);
  RegisterPropertyEditor(TypeInfo(string), TODPriceController,
    'About', TODAboutProperty);
end;

{ TODPriceCtlConnectionNameProperty --------------------------------------}

function TODPriceCtlConnectionNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TODPriceCtlConnectionNameProperty.GetValues(Proc: TGetStrProc);
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
//    Session.GetConnectionNames(st);
    for ix := 0 to st.Count-1 do
      Proc(st[ix]);
  finally
    st.Free;
  end;
end;

{ TODPriceCtlTableNameProperty --------------------------------------}

function TODPriceCtlTableNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TODPriceCtlTableNameProperty.GetValues(Proc: TGetStrProc);
var
  st: TStrings;
  ix: Integer;
  ConnName: String;
  LFDConn: TFDCustomConnection;
begin
  with GetComponent(0) as TODPriceController do
    if ConnectionName <> '' then
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
//        Session.GetTableNames(ConnectionName, '*.*',False, False, st);
        for ix := 0 to st.Count-1 do
          Proc(st[ix]);
      finally
        st.Free;
      end;
    end;
end;

{ TODPriceCtlFieldNameProperty --------------------------------------}

function TODPriceCtlFieldNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TODPriceCtlFieldNameProperty.GetValues(Proc: TGetStrProc);
var
  ix: Integer;
  dt: TODPriceCtlDefTable;
begin
  with (GetComponent(0) as TODPriceFieldNames).PriceController do
    if (ConnectionName <> '') and (PriceTableName <> '') then
    begin
//      dt := TODPriceCtlDefTable.Create(TComponent(GetComponent(0)));
      dt := TODPriceCtlDefTable.Create(nil);
      try
        dt.ConnectionName := ConnectionName;
        dt.TableName := PriceTableName;
        dt.InitFieldDefs;
        for ix := 0 to dt.FieldDefs.Count-1 do
          Proc(dt.FieldDefs[ix].Name);
      finally
        dt.Free;
      end;
    end;
end;

{ TODPriceCtlKeyFieldNamesProperty --------------------------------------}

function TODPriceCtlKeyFieldNamesProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TODPriceCtlKeyFieldNamesProperty.GetValues(Proc: TGetStrProc);
var
  ix: Integer;
  dt: TODPriceCtlDefTable;
begin
  with (GetComponent(0) as TODPriceFieldNames).PriceController do
    if (ConnectionName <> '') and (PriceTableName <> '') then
    begin
//      dt := TODPriceCtlDefTable.Create(TComponent(GetComponent(0)));
      dt := TODPriceCtlDefTable.Create(nil);
      try
        if (ItemDataSet <> nil) and (ItemDataSet is TFDRdbmsDataSet) and
           ((ItemDataSet as TFDRdbmsDataSet).ConnectionName <> EmptyStr) then
          dt.ConnectionName := (ItemDataSet as TFDRdbmsDataSet).ConnectionName
        else
          dt.ConnectionName := ConnectionName;
        if ItemTableName <> '' then
          dt.TableName := ItemTableName
        else
          dt.TableName := PriceTableName;
        dt.IndexDefs.Update;
        for ix := 0 to dt.IndexDefs.Count-1 do
          Proc(dt.IndexDefs[ix].Fields);
      finally
        dt.Free;
      end;
    end;
end;

// ODAboutProperty
function TODAboutProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

procedure TODAboutProperty.Edit;
begin
  MessageDlg('Orbital Decisions ' + ODProductName + ' ' + ODVersion +
    #13#13 + 'My Power Station Technology (Pty) Ltd'#13 +
    'Delphi development, solutions and business components'#13#13 +
    'Addr.:   P.O.Box 1080, Milnerton, 7435, South Africa'#13 +
    'EMail:  components@mypowerstation.biz'#13 +
    'URL:    http://www.orbital.co.za',
    mtInformation, [mbOK], 0);
end;


end.
