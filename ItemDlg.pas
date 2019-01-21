unit ItemDlg;
{
  ItemDlg, part of TODJobSchedule Component - general purpose scheduler.
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
  VCL.StdCtrls, VCL.Buttons, ODSched;

type
  TODJobItemForm = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    Label1: TLabel;
    AvailList: TListBox;
    procedure OKBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure AvailListClick(Sender: TObject);
    procedure AvailListDblClick(Sender: TObject);
  public
    JobItem: TODJobItem;
    Job: TODJob;
    OnInitDialog: TODInitItemDialogEvent;
  end;

var
  ODJobItemForm: TODJobItemForm;

implementation

{$R *.DFM}

procedure TODJobItemForm.FormShow(Sender: TObject);
begin
  if Assigned(OnInitDialog) then
    OnInitDialog(Job.Schedule, Job, JobItem, AvailList.Items);
  with AvailList do
  begin
    MultiSelect := JobItem = nil;
    if not MultiSelect then
    begin            //editing a job item
      if Items.IndexOf(JobItem.Caption) = -1 then
        Items.Add(JobItem.Caption);
      ItemIndex := Items.IndexOf(JobItem.Caption);
    end;
  end;
end;

procedure TODJobItemForm.OKBtnClick(Sender: TObject);
var
  ix: Integer;
begin
  with AvailList do
    if JobItem <> nil then           //editing an existing item
      JobItem.Caption := Items[ItemIndex]
    else if Job <> nil then        //adding new items -
      for ix := 0 to Items.Count-1 do
        if Selected[ix] then
          Job.AddItemFrom(0, Items[ix]);
end;

procedure TODJobItemForm.AvailListClick(Sender: TObject);
begin
  OKBtn.Enabled := AvailList.Items.Count > 0;
end;

procedure TODJobItemForm.AvailListDblClick(Sender: TObject);
begin
  ModalResult := mrOK;
  OKBtnClick(nil);
end;

end.
