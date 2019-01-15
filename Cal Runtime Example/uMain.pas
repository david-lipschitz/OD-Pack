{
  My Power Station Technology (Pty) Ltd - was Orbital Decisions
  P.O.Box 1080, Milnerton 7435, South Africa
  components@mypowerstation.biz
  http://www.orbital.co.za/text/prodlist.htm
  Copyright (c) 1998-2019

  Use at your own risk!
}

unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, ODCalend, Vcl.ExtCtrls;

type
  TForm3 = class(TForm)
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    ODCalendar: TODCalendar;
  end;

var
  Form3: TForm3;

implementation

//uses ;
{$R *.dfm}

procedure TForm3.FormCreate(Sender: TObject);
begin
  ODCalendar := TODCalendar.Create(Self);
  ODCalendar.Align := TAlign.alClient;
//  ODCalendar.
  ODCalendar.Parent := Self;
  ODCalendar.Visible := True;
  Panel1.ControlStyle:= Panel1.ControlStyle - [csParentBackground];
end;

procedure TForm3.Panel1Click(Sender: TObject);
begin
  if Panel1.Color = clGreen then
    Panel1.Color := clRed
  else
    Panel1.Color := clGreen;
end;

end.
