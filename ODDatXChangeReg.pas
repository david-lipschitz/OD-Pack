unit ODDatXChangeReg;
{
  Orbital Decisions Data Exchange Component Suite Version 5.0.0
  This unit registers the Components contained in ODDBImporter.pas and
  ODDBExporter.pas

  My Power Station Technology (Pty) Ltd - was Orbital Decisions
  P.O.Box 1080, Milnerton 7435, South Africa
  components@mypowerstation.biz
  http://www.orbital.co.za/text/prodlist.htm
  Copyright (c) 1998-2019

  Use at your own risk!
}

interface

{$INCLUDE ODDatXSet.pas}

uses DesignIntf, DesignEditors, Vcl.Dialogs;

type
  TFileProperty = class(TStringProperty)
  public
    { Public declarations }
    DialogOptions: TOpenOptions;
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  // Decendants of my TFileProperty to change the dialog box's options
  TExportFileProp = class(TFileProperty)
  public
    procedure Edit; override;
  end;

  // Importer stuff
  TImportFileProp = class(TFileProperty)
  public
    procedure Edit; override;
  end;

  TExceptionFileProp = class(TFileProperty)
  public
    procedure Edit; override;
  end;

  TControlFileProp = class(TFileProperty)
  public
    procedure Edit; override;
  end;

  // common

  // Component About box
  TODAboutProperty = class(TStringProperty)
  public
    ProductName, Version : String;
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;


procedure Register;

implementation

uses System.Classes, System.SysUtils, Vcl.Forms,
     ODDBImporter, ODDBExporter,
     {$IFDEF Full} ODDBCommon;
     {$ELSE} ODDBCommon, ODDemo; {$ENDIF}

procedure Register;
begin
  // Register TODDBImporter
  RegisterComponents('Orbital', [TODDBImporter]);
  RegisterPropertyEditor(TypeInfo(TFileName), TODDBImporter, 'ImportFile', TImportFileProp);
  RegisterPropertyEditor(TypeInfo(TFileName), TODDBImporter, 'ExceptionFile', TExceptionFileProp);
  RegisterPropertyEditor(TypeInfo(TFileName), TODDBImporter, 'ControlFile', TControlFileProp);
  RegisterPropertyEditor(TypeInfo(String), TODDBImporter, 'About', TODAboutProperty);
  // Register TODDBExporter
  RegisterComponents('Orbital', [TODDBExporter]);
  RegisterPropertyEditor(TypeInfo(TFileName), TODDBExporter, 'ExportFile', TExportFileProp);
  RegisterPropertyEditor(TypeInfo(TFileName), TODDBExporter, 'ControlFile', TExportFileProp);
  RegisterPropertyEditor(TypeInfo(String), TODDBExporter, 'About', TODAboutProperty);
  { The trial version has only the following restrictions:
    It can only be used from within Delphi
    It may only be used for 30 days after installation }
  {$IFDEF Trial}
    InstallTrial;
  {$ENDIF}
end;

function TFileProperty.GetAttributes: TPropertyAttributes;
begin
  // Set the component property FileName to use a Dialog box in addition to
  // normal string editing.
  Result := [paDialog];  // Display a dialog
end;


procedure TFileProperty.Edit;
var
  OpenDialog : TOpenDialog;

begin
  // Define the result of clicking the ellipses for the FileName property
  { Create the TOpenDialog }
  OpenDialog := TOpenDialog.Create(Application);
  try
    { Show all files }
    OpenDialog.Filter := 'All text files (*.*)|*.*|Comma Separated Values ' +
      '(*.csv)|*.csv|Delimited Value Format (*.dvf)|*.dvf';
    OpenDialog.Options := DialogOptions;
    OpenDialog.Title := 'Select a text file or create one...';
    { If the user selects a file then assign it to the property }
    if OpenDialog.Execute then
      SetStrValue(OpenDialog.FileName);
    finally
      OpenDialog.Free;
  end;
end;

//
// TFileProperty Implementation
//

procedure TExportFileProp.Edit;
begin
  DialogOptions := [ofHideReadOnly, ofCreatePrompt];
  inherited Edit;
end;

// import
procedure TImportFileProp.Edit;
begin
  DialogOptions := [ofHideReadOnly];
  inherited Edit;
end;

procedure TExceptionFileProp.Edit;
begin
  DialogOptions := [ofHideReadOnly];
  inherited Edit;
end;

procedure TControlFileProp.Edit;
begin
  inherited Edit;
end;

// common

{ TODAboutProperty Object -----------------------------------------------------}

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
    'EMail:  ' + ODSalesEMail + #13 +
    {$IFDEF Trial}
      'URL:    ' + ODProductWebpage + #13#13 +
      'Trial Version information:  ' + GEDRAS,
    {$ELSE}
      'URL:    ' + ODProductWebpage,
    {$ENDIF}
    mtInformation, [mbOK], 0);
end;


end.
