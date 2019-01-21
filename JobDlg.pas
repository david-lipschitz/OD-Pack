unit JobDlg;
{
  JobDlg, part of TODJobSchedule Component - general purpose scheduler.
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
  VCL.ComCtrls, VCL.StdCtrls, VCL.Buttons, ODSched;

type
  TODJobForm = class(TForm)
    GroupBox1: TGroupBox;
    UpperCombo: TComboBox;
    LowerCombo: TComboBox;
    UpperLabel: TLabel;
    LowerLabel: TLabel;
    StartDatePicker: TDateTimePicker;
    FinishDatePicker: TDateTimePicker;
    Label6: TLabel;
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    Label5: TLabel;
    UpperEdit: TEdit;
    LowerEdit: TEdit;
    procedure FormShow(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure CtrlChange(Sender: TObject);
    procedure DatePickerChange(Sender: TObject);
  public
    Job: TODJob;
    Schedule: TODJobSchedule;
    OnInitDialog: TODInitJobDialogEvent;
    OnInitItemDialog: TODInitItemDialogEvent;
  end;

var
  ODJobForm: TODJobForm;

implementation

uses ItemDlg;

{$R *.DFM}

procedure TODJobForm.FormShow(Sender: TObject);
begin
  if Assigned(OnInitDialog) then        //get the available items
  begin
    OnInitDialog(Schedule, Job, UpperCombo.Items, LowerCombo.Items);
    LowerCombo.Visible := LowerCombo.Items.Count > 0;
    LowerLabel.Visible := LowerCombo.Visible;
    if LowerLabel.Visible then
      UpperLabel.Caption := '&Upper Caption'
    else
      UpperLabel.Caption := '&Caption';
  end
  else
  begin                   //allow any values to be entered -
    UpperCombo.Hide;
    LowerCombo.Hide;
    UpperEdit.Show;
    LowerEdit.Show;
    UpperLabel.FocusControl := UpperEdit;
    LowerLabel.FocusControl := LowerEdit;
  end;
  if Job <> nil then          //if an edit then set current values -
  begin
    with Job do begin
      with UpperCombo do
        if Visible then
        begin
          if Items.IndexOf(UpperCaption) = -1 then
            Items.Add(UpperCaption);                //add to those available
          ItemIndex := Items.IndexOf(UpperCaption);
        end
        else UpperEdit.Text := UpperCaption;
      with LowerCombo do
        if Visible then
        begin
          LowerLabel.FocusControl := LowerCombo;
          if Items.IndexOf(LowerCaption) = -1 then
            Items.Add(LowerCaption);                //add to those available
          ItemIndex := Items.IndexOf(LowerCaption);
        end
        else LowerEdit.Text := LowerCaption;
      if StartDate < Now then
        StartDatePicker.Enabled := False{ else    //already started
        StartDatePicker.MinDate := Now;          //cannot start before today
      FinishDatePicker.MinDate := StartDate};
      StartDatePicker.Date := StartDate;
      FinishDatePicker.Date := FinishDate;
    end;
  end
  else   //adding a new job -
  begin
{   StartDatePicker.MinDate := Now;
    FinishDatePicker.MinDate := Now;}
    StartDatePicker.Date := Now;
    FinishDatePicker.Date := Now;
  end;
end;

procedure TODJobForm.CtrlChange(Sender: TObject);
begin
  OKBtn.Enabled := not
    (UpperCombo.Visible and (UpperCombo.ItemIndex = -1)) or
    (LowerCombo.Visible and (LowerCombo.ItemIndex = -1)) or
    (UpperEdit.Visible and (UpperEdit.Text = ''));
end;

procedure TODJobForm.OKBtnClick(Sender: TObject);
var
  sUpper, sLower: string;
begin
  if UpperCombo.Visible then
    sUpper := UpperCombo.Items[UpperCombo.ItemIndex] else
    sUpper := UpperEdit.Text;
  if LowerCombo.Visible then
    sLower := LowerCombo.Items[LowerCombo.ItemIndex] else
    sLower := LowerEdit.Text;
  if Job <> nil then     //editing an existing job -
    with Job do
    begin
      Uppercaption := sUpper;
      LowerCaption := sLower;
      StartDate := StartDatePicker.Date;
      FinishDate := FinishDatePicker.Date;
    end
  else      //adding a new job -
    (Owner as TODJobSchedule).AddJobFrom(0, sUpper, sLower,
      StartDatePicker.Date, FinishDatePicker.Date);
end;

procedure TODJobForm.DatePickerChange(Sender: TObject);
begin
  if FinishDatePicker.Date < StartDatePicker.Date then
    FinishDatePicker.Date := StartDatePicker.Date;
{ FinishDatePicker.MinDate := StartDatePicker.Date;}
  CtrlChange(nil);
end;

end.
