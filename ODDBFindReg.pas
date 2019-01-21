unit ODDBFindReg;
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
  DesignIntf, DesignEditors, ODDBFind;

procedure Register;

implementation

Uses
  Classes, Vcl.Dialogs;

type
  TODAboutProperty = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  TODFindMenuEditor = class(TComponentEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

const
  ODVersion = '5.0.0';
  DemoVersion = False;
  HintTrigger = 5;

procedure Register;
begin
  RegisterComponents('Orbital', [TODFindMenu]);
  RegisterPropertyEditor(TypeInfo(string), TODFindMenu,
    'About', TODAboutProperty);
  RegisterComponentEditor(TODFindMenu, TODFindMenuEditor);
end;

function TODAboutProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

procedure TODAboutProperty.Edit;
begin
  MessageDlg('Orbital Decisions FindMenu Component ' + ODVersion +
    #13#13 + 'My Power Station Technology (Pty) Ltd'#13 +
    'Delphi development, solutions and business components'#13#13 +
    'Addr.:   P.O.Box 1080, Milnerton, 7435, South Africa'#13 +
    'EMail:  components@mypowerstation.biz'#13 +
    'URL:    http://www.orbital.co.za/text/prodlist.htm',
    mtInformation, [mbOK], 0);
end;

procedure TODFindMenuEditor.Edit;
begin
  ExecuteVerb(0);
end;

procedure TODFindMenuEditor.ExecuteVerb(Index: Integer);
begin
  with Component as TODFindMenu do
  begin
    if DataSource = nil then
      raise EComponentError.Create('No datasource assigned');
    if DataSource.DataSet = nil then
      raise EComponentError.Create('Invalid dataset');
    if not DataSource.DataSet.Active then
      raise EComponentError.Create('Dataset not active');
    case Index of
      0: DoFind(nil);
      1: DoFindNext(nil);
    end;
  end;
end;

function TODFindMenuEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Find';
    1: Result := 'Find Next';
  end;
end;

function TODFindMenuEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

end.
