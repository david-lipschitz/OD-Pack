{
  Copyright (c) 1998-2019
  My Power Station Technology (Pty) Ltd - was Orbital Decisions
  P.O.Box 1080, Milnerton 7435, South Africa
  components@mypowerstation.biz
  http://www.orbital.co.za/text/prodlist.htm

Use at your own risk!
}

unit TestUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons, ComCtrls, Db, DBCtrls, Grids, DBGrids,
  ODCalend, ODPopCal, ODDBCal, ODTime, ShellAPI, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, FireDAC.Stan.Async, FireDAC.DApt,
  FireDAC.UI.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Phys,
  FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs,
  FireDAC.Comp.Client, FireDAC.Comp.DataSet, FireDAC.VCLUI.Wait, FireDAC.Comp.UI;

type
  TTestForm = class(TForm)
    ODCalendarDialog1: TODCalendarDialog;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Bevel1: TBevel;
    Image1: TImage;
    Label2: TLabel;
    Bevel2: TBevel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Memo1: TMemo;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    ODCalendar0: TODCalendar;
    ODPopupCalendar1: TODPopupCalendar;
    Button1: TButton;
    Edit1: TEdit;
    GroupBox1: TGroupBox;
    SingleDateCheck: TCheckBox;
    StatusCheck: TCheckBox;
    MondayCheck: TCheckBox;
    GroupBox2: TGroupBox;
    YearToDateBtn: TSpeedButton;
    Last2MonthsBtn: TSpeedButton;
    Last6MonthsBtn: TSpeedButton;
    ThisMonthBtn: TSpeedButton;
    Label8: TLabel;
    ODDBCalendar0: TODDBCalendar;
    DBNavigator1: TDBNavigator;
    ODDBPopupCalendar1: TODDBPopupCalendar;
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    PlainCheck: TCheckBox;
    Label9: TLabel;
    GotoStartButton: TButton;
    TimeCheck: TCheckBox;
    TabSheet4: TTabSheet;
    Label10: TLabel;
    Memo2: TMemo;
    Edit2: TEdit;
    ODDBTimePicker1: TODDBTimePicker;
    CheckBox4: TCheckBox;
    Label12: TLabel;
    WeeksCheck: TCheckBox;
    Label11: TLabel;
    FDTable1: TFDTable;
    FDConnection1: TFDConnection;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    FDTable1ID: TIntegerField;
    FDTable1StartDate: TDateField;
    FDTable1FinishDate: TDateField;
    FDTable1ATime: TStringField;
    ODCalendar1: TODCalendar;
    ODDBCalendar1: TODDBCalendar;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure SingleDateCheckClick(Sender: TObject);
    procedure StatusCheckClick(Sender: TObject);
    procedure MondayCheckClick(Sender: TObject);
    procedure CalendarSetupDay(Sender: TObject; Date: TDateTime;
      var AColor: TColor; var AHint: String);
    procedure CalendarSelectYear(Sender: TObject; Year: Integer;
      var YearStart, YearFinish: TDateTime);
    procedure CalendarDayDblClick(Sender: TObject; ADate: TDateTime);
    procedure YearToDateBtnClick(Sender: TObject);
    procedure Last2MonthsBtnClick(Sender: TObject);
    procedure Last6MonthsBtnClick(Sender: TObject);
    procedure ThisMonthBtnClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure PlainCheckClick(Sender: TObject);
    procedure CalendarChange(Sender: TObject);
    procedure ODPopupCalendar1StartClick(Sender: TObject;
      var ADate: TDateTime);
    procedure GotoStartButtonClick(Sender: TObject);
    procedure ODCalendar0StartClick(Sender: TObject; var ADate: TDateTime);
    procedure TimeCheckClick(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure WeeksCheckClick(Sender: TObject);
    procedure Edit2Click(Sender: TObject);
    procedure TabSheet4Show(Sender: TObject);
    procedure FDConnection1BeforeConnect(Sender: TObject);
  private
    procedure SetEdit;
  end;

var
  TestForm: TTestForm;

implementation

{$R *.DFM}

procedure FindDB(ADBName: String; AConn: TFDConnection);
var
  APath, ADBPath: String;
begin
  APath := '';
  if not FileExists(AConn.Params.Database) then
  begin
    while Length(APath) < 25 do
    begin
      ADBPath := APath + ADBName;
      if FileExists(ADBPath) then break;
      ADBPath := APath + 'Data\' + ADBName;
      if FileExists(ADBPath) then break;
      APath:= '..\' + APath;
    end;
    if FileExists(ADBPath) then
      AConn.Params.Database := ADBPath
    else
    begin
      AConn.Params.Database := ExpandFileName(ADBName);
      ShowMessage('Can''t find sqlite database at ' + AConn.Params.Database);
      Application.Terminate;
      Abort;
    end;
  end;
end;

procedure TTestForm.FDConnection1BeforeConnect(Sender: TObject);
begin
  FindDB('Dates.sqlite', FDConnection1);
end;

procedure TTestForm.FormShow(Sender: TObject);
begin
  PageControl1.ActivePage := TabSheet1;
{ ODCalendar1.StartDate := EncodeDate(1998, 1, 15);
  ODCalendar1.FinishDate := Date;}
{ ODPopupCalendar1.StartDate := Date - 14;
  ODPopupCalendar1.FinishDate := Date;}
 {ODPopupCalendar1.StartDate := 0;
  ODPopupCalendar1.FinishDate := 0;
  ODPopupCalendar1.DisplayDate := EncodeDate(1999, 6, 1);}
end;

procedure TTestForm.Button1Click(Sender: TObject);
begin
  if ODCalendarDialog1.Execute then SetEdit;
end;

procedure TTestForm.SetEdit;
var
  FormatStr: string;
begin
  with ODCalendarDialog1 do
    if StartDate > 0 then
    begin
      if UseTime then
        FormatStr := DateFormat + ' hh:mm'
      else
        FormatStr := DateFormat;
      if SingleDate then
        Edit1.Text := FormatDateTime(FormatStr, StartDate)
      else
        Edit1.Text := FormatDateTime(FormatStr, StartDate) +
          ' - ' + FormatDateTime(FormatStr, FinishDate);
    end
    else Edit1.Text := '';
end;

procedure TTestForm.SingleDateCheckClick(Sender: TObject);
begin
  with SingleDateCheck do
  begin
    ODCalendar1.SingleDate := Checked;
    ODPopupCalendar1.SingleDate := Checked;
    ODCalendarDialog1.SingleDate := Checked;
    if Checked then
      ODPopupCalendar1.PopupCaption := 'Select A Date'
    else
      ODPopupCalendar1.PopupCaption := 'Select Date Range';
    ODCalendarDialog1.Caption := ODPopupCalendar1.PopupCaption;
  end;
  SetEdit;
end;

procedure TTestForm.StatusCheckClick(Sender: TObject);
begin
  ODCalendar1.ShowStatus := StatusCheck.Checked;
  ODPopupCalendar1.ShowStatus := StatusCheck.Checked;
  ODCalendarDialog1.ShowStatus := StatusCheck.Checked;
end;

procedure TTestForm.MondayCheckClick(Sender: TObject);
begin
  ODCalendar1.StartOnMonday := MondayCheck.Checked;
  ODPopupCalendar1.StartOnMonday := MondayCheck.Checked;
  ODCalendarDialog1.StartOnMonday := MondayCheck.Checked;
end;

procedure TTestForm.CalendarSetupDay(Sender: TObject; Date: TDateTime;
  var AColor: TColor; var AHint: String);
begin                                    //customise certain days -
  if Date = EncodeDate(2018, 8, 10) then
  begin
    AColor := clFuchsia;
    AHint := 'Happy birthday!';
  end
  else if Date = EncodeDate(2018, 8, 22) then
  begin
    AColor := clRed;
    AHint := 'Important!';
  end;
end;

procedure TTestForm.CalendarSelectYear(Sender: TObject; Year: Integer;
  var YearStart, YearFinish: TDateTime);
begin
  if Year = 2018 then       //use custom year start and end for 2018
  begin                              //as for a business year
    YearStart := EncodeDate(2018, 1, 1);
    YearFinish := EncodeDate(2018, 12, 31);
  end;
end;

procedure TTestForm.CalendarDayDblClick(Sender: TObject;
  ADate: TDateTime);
begin
  ShowMessage(DateToStr(ADate));
end;

procedure TTestForm.YearToDateBtnClick(Sender: TObject);
var
  yr, mo, dy: Word;
begin
  DecodeDate(Date, yr, mo, dy);
  if IsLeapYear(yr) then
    ODCalendar1.StartDate := Date - 366
  else
    ODCalendar1.StartDate := Date - 365;
  ODCalendar1.FinishDate := Date;
  ODCalendar1.DisplayDate := Date;
end;

procedure TTestForm.Last2MonthsBtnClick(Sender: TObject);
var
  yr, mo, dy: Word;
  m1, m2: Integer;
begin
  DecodeDate(Date, yr, mo, dy);
  if mo = 1 then
  begin
   Dec(yr);
   mo := 12;
  end
  else Dec(mo);
  m1 := ODCalendar1.DaysInMonth(mo, yr);
  if mo = 1 then
  begin
   Dec(yr);
   mo := 12;
  end
  else Dec(mo);
  m2 := ODCalendar1.DaysInMonth(mo, yr);
  ODCalendar1.StartDate := Date - m1 - m2;
  ODCalendar1.FinishDate := Date;
  ODCalendar1.DisplayDate := Date;
end;

procedure TTestForm.Last6MonthsBtnClick(Sender: TObject);
var
  yr, mo, dy: Word;
  dc, ix: Integer;
begin
  DecodeDate(Date, yr, mo, dy);
  dc := 0;
  for ix := 0 to 5 do
  begin
    if Mo = 1 then
    begin
     Dec(yr);
     mo := 12;
    end
    else Dec(mo);
    Inc(dc, ODCalendar1.DaysInMonth(mo, yr));
  end;
  ODCalendar1.StartDate := Date - dc;
  ODCalendar1.FinishDate := Date;
  ODCalendar1.DisplayDate := Date;
end;

procedure TTestForm.TabSheet4Show(Sender: TObject);
begin
//  if Memo2.Lines.Count = 0 then
//  begin
//    Memo2.Lines.LoadFromFile('..\products.rtf');
//  end;
end;

procedure TTestForm.ThisMonthBtnClick(Sender: TObject);
begin
  ODCalendar1.DisplayDate := Date;
end;

procedure TTestForm.PageControl1Change(Sender: TObject);
begin
  FDTable1.Active := PageControl1.ActivePage.TabIndex = 2;
end;

procedure TTestForm.PlainCheckClick(Sender: TObject);
begin
  with PlainCheck do
  begin
    ODCalendar1.Plain := Checked;
    ODPopupCalendar1.Plain := Checked;
    ODCalendarDialog1.Plain := Checked;
    if Checked then
      ODCalendar1.BevelEdge := bvLowered
    else
      ODCalendar1.BevelEdge := bvNone;
  end;
end;

procedure TTestForm.TimeCheckClick(Sender: TObject);
begin
  with TimeCheck do
  begin
    ODCalendar1.UseTime := Checked;
    ODPopupCalendar1.UseTime := Checked;
    ODCalendarDialog1.UseTime := Checked;
  end;
  SetEdit;  //update Edit1
end;

procedure TTestForm.CalendarChange(Sender: TObject);
begin
  ShowMessage('Change');
end;

procedure TTestForm.ODPopupCalendar1StartClick(Sender: TObject;
  var ADate: TDateTime);
begin
  ShowMessage('Start click');
end;

procedure TTestForm.GotoStartButtonClick(Sender: TObject);
begin
  ODCalendar1.DisplayDate := ODCalendar1.StartDate;
end;

procedure TTestForm.ODCalendar0StartClick(Sender: TObject;
  var ADate: TDateTime);
begin
  GotoStartButton.Enabled := True;
end;

procedure TTestForm.CheckBox4Click(Sender: TObject);
begin
  ODDBCalendar1.UseTime := CheckBox4.Checked;
  ODDBPopupCalendar1.UseTime := CheckBox4.Checked;
  if CheckBox4.Checked then
  begin
    FDTable1StartDate.DisplayFormat := 'dd/mm/yy hh:mm';
    FDTable1FinishDate.DisplayFormat := 'dd/mm/yy hh:mm';
  end
  else
  begin
    FDTable1StartDate.DisplayFormat := 'd mmm yyyy';
    FDTable1FinishDate.DisplayFormat := 'd mmm yyyy';
  end;
end;

procedure TTestForm.WeeksCheckClick(Sender: TObject);
begin
  ODCalendar1.ShowWeeks := WeeksCheck.Checked;
  ODPopupCalendar1.ShowWeeks := WeeksCheck.Checked;
  ODCalendarDialog1.ShowWeeks := WeeksCheck.Checked;
end;

procedure TTestForm.Edit2Click(Sender: TObject);
begin
  ShellExecute(0, 'open', 'http://www.orbital.co.za/text/prodlist.htm', nil, '', 0);
end;

end.
