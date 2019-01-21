unit ODSchedReg;
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
  DesignIntf, DesignEditors;

procedure Register;

implementation
uses
 Classes,{Graphics,Controls,}Vcl.Dialogs, ODSched;

type
  TODAboutProperty = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

const
  ODVersion = '5.0.0';
  Restricted = False;
  MaxItems = 5;
  MaxJobs = 5;
  MaxSchedules = 3;

procedure Register;
begin
  RegisterComponents('Orbital', [TODJobSchedule, TODScheduleBox]);
  RegisterPropertyEditor(TypeInfo(string), TODJobSchedule,
    'About', TODAboutProperty);
  RegisterPropertyEditor(TypeInfo(string), TODScheduleBox,
    'About', TODAboutProperty);
end;

function TODAboutProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

procedure TODAboutProperty.Edit;
begin
  MessageDlg('Orbital Decisions Scheduling Components ' + ODVersion +
    #13#13 + 'My Power Station Technology (Pty) Ltd'#13 +
    'Delphi development, solutions and business components'#13#13 +
    'Addr.:   P.O.Box 1080, Milnerton, 7435, South Africa'#13 +
    'EMail:  components@mypowerstation.biz'#13 +
    'URL:    http://www.orbital.co.za',
    mtInformation, [mbOK], 0);
end;

end.
