unit ODDatPrnReg;
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
  DesignIntF, DesignEditors;

procedure Register;

implementation

uses
  System.Classes, Vcl.Dialogs, ODDatPrn;

type
  TODAboutProperty = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

const
  EvaluationVersion = False;
  ODVersion = '2.1.3';

procedure Register;
begin
  RegisterComponents('Orbital', [TODDataPrinter]);
  RegisterPropertyEditor(TypeInfo(string), TODDataPrinter,
    'About', TODAboutProperty);
end;

function TODAboutProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

procedure TODAboutProperty.Edit;
begin
  MessageDlg('Orbital Decisions DataPrinter Component ' + ODVersion +
    #13#13 + 'My Power Station Technology (Pty) Ltd'#13 +
    'Delphi development, solutions and business components'#13#13 +
    'Addr.:   P.O.Box 1080, Milnerton, 7435, South Africa'#13 +
    'EMail:  components@mypowerstation.biz'#13 +
    'URL:    http://www.orbital.co.za',
    mtInformation, [mbOK], 0);
end;

{Orbital Decisions (Pty) Ltd to My Power Station Technology (Pty) Ltd
Textile Factory, Stock Control and Loyalty Systems to Delphi and Oracle Software Development and Energy Keynote Speaker
Email: components@orbital.co.za
Copyright (c): 1998-2015 by My Power Station Technology (Pty) Ltd
}
end.
