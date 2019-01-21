{
  My Power Station Technology (Pty) Ltd - was Orbital Decisions
  P.O.Box 1080, Milnerton 7435, South Africa
  components@mypowerstation.biz
  http://www.orbital.co.za/text/prodlist.htm
  Copyright (c) 1998-2019

  Use at your own risk!

  Create a Calendar Component at run time
}

program CalRuntime;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {Form3},
  ODCalend in '..\ODCalend.pas',
  ODTime in '..\ODTime.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
