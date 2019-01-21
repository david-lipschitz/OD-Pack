unit ODSched;
{
  TODJobSchedule Component - general purpose scheduler.
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
  VCL.StdCtrls, VCL.ComCtrls, VCL.ExtCtrls, VCL.Menus, VCL.Buttons;

type
  TODJob = class;
  TODJobSchedule = class;

  TODItemMenuCaptions = class(TObject)
  private
    FEdit, FRemove: string;
  public
    property Edit: string read FEdit write FEdit;
    property Remove: string read FRemove write FRemove;
  end;

  TODJobItem = class(TCustomStaticText)
  private
    FItemNo: Longint;
    FEditMenu, FRemoveMenu: TMenuItem;
    FMenuCaptions: TODItemMenuCaptions;
    function GetJob: TODJob;
  protected
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); override;
    procedure DoEndDrag(Source: TObject; X, Y: Integer); override;
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DoClick(Sender: TObject);
    procedure DoDblClick(Sender: TObject);
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DoItemEdit(Sender: TObject);
    procedure DoItemRemove(Sender: TObject);
    procedure DoPopup(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    property Job: TODJob read GetJob;
    property ItemNo: Longint read FItemNo write FItemNo default 0;
    property MenuCaptions: TODItemMenuCaptions read FMenuCaptions write FMenuCaptions;
    property Caption;
    property Color;
    property Font;
  end;

  TODJobMenuCaptions = class(TObject)
  private
    FAddItem, FClearItems, FEdit, FRemove: string;
  public
    property AddItem: string read FAddItem write FAddItem;
    property ClearItems: string read FClearItems write FClearItems;
    property Edit: string read FEdit write FEdit;
    property Remove: string read FRemove write FRemove;
  end;

  TODJob = class(TCustomPanel)
  private
    FJobNo: Longint;
    FStartDate, FFinishDate: TDate;
    FItems: TList;
    FExpanded, FAddingFrom: Boolean;
    FMenuCaptions: TODJobMenuCaptions;
    FDateFormat: string;
    FAddItemMenu, FClearItemsMenu, FEditMenu, FRemoveMenu,
      FSeperator: TMenuItem;
    function GetSchedule: TODJobSchedule;
    function GetUpperCaption: string;
    procedure SetUpperCaption(const Value: string);
    function GetLowerCaption: string;
    procedure SetLowerCaption(const Value: string);
    function GetStatus: string;
    procedure SetStatus(const Value: string);
    procedure SetStartDate(Value: TDate);
    procedure SetFinishDate(Value: TDate);
    function GetFont: TFont;
    procedure SetFont(Value: TFont);
    function GetColor: TColor;
    procedure SetColor(Value: TColor);
    function GetDateFont: TFont;
    procedure SetDateFont(Value: TFont);
    function GetDateColor: TColor;
    procedure SetDateColor(Value: TColor);
    function GetStatusFont: TFont;
    procedure SetStatusFont(Value: TFont);
    function GetStatusColor: TColor;
    procedure SetStatusColor(Value: TColor);
    function GetItemCount: Integer;
    function GetItem(Index: Integer): TODJobItem;
    procedure SetItem(Index: Integer; Value: TODJobItem);
    procedure SetExpanded(Value: Boolean);
    procedure SetDateFormat(const Value: string);
    procedure SetDates;
  protected
    FUpperLabel, FLowerLabel: TLabel;
    FDateText, FStatusText: TStaticText;
    procedure Resize; override;
    procedure DoDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean);
    procedure DoDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    procedure DoEndDragging(Sender, Target: TObject; X, Y: Integer);
    procedure ItemDragDrop(Sender: TObject; Item: TODJobItem);
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DoClick(Sender: TObject);
    procedure DoDblClick(Sender: TObject);
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DoJobEdit(Sender: TObject);
    procedure DoJobRemove(Sender: TObject);
    procedure DoItemAdd(Sender: TObject);
    procedure DoItemsClear(Sender: TObject);
    procedure DoPopup(Sender: TObject);
    procedure ArrangeItems;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddItem(Item: TODJobItem);
    function AddNewItem: TODJobItem;
    function AddItemFrom(AItemNo: Integer; const ACaption: string): TODJobItem;
    procedure InsertItem(Index: Integer; Item: TODJobItem);
    procedure DeleteItem(Index: Integer; ToFree: Boolean);
    function RemoveItem(Item: TODJobItem; ToFree: Boolean): Integer;
    procedure ClearItems(ToFree: Boolean);
    procedure MoveItem(CurIndex, NewIndex: Integer);
    function ItemAt(Index: Integer): TODJobItem;
    function IndexOfItem(Item: TODJobItem): Integer;
    function ItemByNo(AItemNo: Longint): TODJobItem;
    function ItemByCaption(const ACaption: string): TODJobItem;
    property Schedule: TODJobSchedule read GetSchedule;
    property JobNo: Longint read FJobNo write FJobNo default 0;
    property UpperCaption: string read GetUpperCaption write SetUpperCaption;
    property LowerCaption: string read GetLowerCaption write SetLowerCaption;
    property Status: string read GetStatus write SetStatus;
    property StatusFont: TFont read GetStatusFont write SetStatusFont;
    property StatusColor: TColor read GetStatusColor write SetStatusColor default cl3DLight;
    property Font: TFont read GetFont write SetFont;
    property Color: TColor read GetColor write SetColor;
    property DateFont: TFont read GetDateFont write SetDateFont;
    property DateColor: TColor read GetDateColor write SetDateColor default clAqua;
    property StartDate: TDate read FStartDate write SetStartDate;
    property FinishDate: TDate read FFinishDate write SetFinishDate;
    property ItemCount: Integer read GetItemCount;
    property Items[Index: Integer]: TODJobItem read GetItem write SetItem; default;
    property Expanded: Boolean read FExpanded write SetExpanded;
    property MenuCaptions: TODJobMenuCaptions read FMenuCaptions write FMenuCaptions;
    property DateFormat: string read FDateFormat write SetDateFormat;
  end;

  TODScheduleEvent = procedure(Schedule: TODJobSchedule) of object;
  TODJobDragEvent = procedure(Schedule: TODJobSchedule; Job: TODJob;
    Index: Integer; var Accept: Boolean) of object;
  TODItemDragEvent = procedure(Schedule: TODJobSchedule; Job: TODJob;
    Item: TODJobItem; Index: Integer; var Accept: Boolean) of object;
  TODJobEvent = procedure(Schedule: TODJobSchedule; Job: TODJob) of object;
  TODItemEvent = procedure(Schedule: TODJobSchedule; Job: TODJob;
    Item: TODJobItem) of object;

  TODInitItemDialogEvent = procedure(Schedule: TODJobSchedule; Job: TODJob;
    Item: TODJobItem; CaptionList: TStrings) of object;
  TODInitJobDialogEvent = procedure(Schedule: TODJobSchedule; Job: TODJob;
    UpperCaptionList, LowerCaptionList: TStrings) of object;

  TODScheduleMenuCaptions = class(TPersistent)
  private
    FAddItem, FEditItem, FRemoveItem, FClearItems,
      FAddJob, FEditJob, FRemoveJob, FClearJobs, FExpanded: string;
  public
    constructor Create;
  published
    property AddItem: string read FAddItem write FAddItem;
    property EditItem: string read FEditItem write FEditItem;
    property RemoveItem: string read FRemoveItem write FRemoveItem;
    property ClearItems: string read FClearItems write FClearItems;
    property AddJob: string read FAddJob write FAddJob;
    property EditJob: string read FEditJob write FEditJob;
    property RemoveJob: string read FRemoveJob write FRemoveJob;
    property ClearJobs: string read FClearJobs write FClearJobs;
    property Expanded: string read FExpanded write FExpanded;
  end;

  TODJobSchedule = class(TCustomPanel)
  private
    FScheduleNo: Longint;
    FJobs: TList;
    FHeaderLabel, FFooterLabel: TLabel;
    FScrollBox: TScrollBox;
    FScrollTimer: TTimer;
    FExpanded, FExpandAll, {FAutoAddItems,} FAddingFrom,
      FAdjustWidth, FResizing, FAutoScroll, FScrollingUp: Boolean;
    {FMaxWidth,} FJobWidth: Integer;
    FMenuCaptions: TODScheduleMenuCaptions;
    FDateFormat: string;
    FAddMenu, FClearMenu, FExpandMenu, FSeperator: TMenuItem;
    FOnBeforeJobAdd, FOnDoJobAdd, FOnAfterJobRemove, FOnAfterJobFree: TODScheduleEvent;
    FOnAfterJobAdd, FOnBeforeJobEdit, FOnAfterJobEdit, FOnBeforeJobRemove,
      FOnBeforeJobFree, FOnDoJobEdit, FOnJobDragDrop, FOnBeforeItemAdd,
      FOnDoItemAdd, FOnAfterItemRemove, FOnAfterItemFree, FOnJobClick: TODJobEvent;
    FOnAfterItemAdd, FOnAfterItemEdit, FOnBeforeItemEdit, FOnBeforeItemRemove,
      FOnBeforeItemFree, FOnDoItemEdit, FOnItemDragDrop, FOnItemClick: TODItemEvent;
    FOnJobDragOver: TODJobDragEvent;
    FOnItemDragOver: TODItemDragEvent;
    FOnInitItemDialog: TODInitItemDialogEvent;
    FOnInitJobDialog: TODInitJobDialogEvent;
    FFormatSettigns: TFormatSettings;
    function GetAbout: string;
    procedure SetAbout(Value: string);
    function GetHeader: string;
    function GetHeaderFont: TFont;
    function GetHeaderColor: TColor;
    function GetFooter: string;
    function GetFooterFont: TFont;
    function GetFooterColor: TColor;
    function GetClientHeight: Integer;
    function GetClientWidth: Integer;
    procedure SetHeader(const Value: string);
    procedure SetHeaderFont(Value: TFont);
    procedure SetHeaderColor(Value: TColor);
    procedure SetFooter(const Value: string);
    procedure SetFooterFont(Value: TFont);
    procedure SetFooterColor(Value: TColor);
    procedure SetExpanded(Value: Boolean);
    procedure SetAdjustWidth(Value: Boolean);
    procedure SetJobWidth(Value: Integer);
//  procedure SetMaxWidth(Value: Integer);
    function GetWidth: Integer;
    procedure SetWidth(Value: Integer);
    function GetJobCount: Integer;
    function GetJob(Index: Integer): TODJob;
    procedure SetJob(Index: Integer; Value: TODJob);
    function GetScrollInterval: Cardinal;
    procedure SetScrollInterval(Value: Cardinal);
  protected
    procedure DoClick(Sender: TObject);
    procedure DoDblClick(Sender: TObject);
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); override;
    procedure DoDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure DoDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    procedure DoEndDragging(Sender, Target: TObject; X, Y: Integer);
    procedure JobDragOver(Sender, Job: TODJob; var Accept: Boolean);
    procedure ItemDragOver(Sender: TODJob; Item: TODJobItem; Index: Integer;
      var Accept: Boolean);
    procedure JobDragDrop(Sender: TObject; Job: TODJob);
    procedure ItemDragDrop(Sender: TObject; Job: TODJob; Item: TODJobItem);
    procedure ItemAdded(Job: TODJob; Item: TODJobItem);
    procedure ItemRemoved(Job: TODJob);
    procedure ItemFreed(Job: TODJob);
    procedure ArrangeJobs;
    procedure ScrollTimed(Sender: TObject);
    procedure DoJobAdd(Sender: TObject);
    procedure DoJobsClear(Sender: TObject);
    procedure DoRemoveItem(Sender: TObject; Item: TODJobItem);
    procedure DoFreeItem(Sender: TObject; Item: TODJobItem);
    procedure DoExpanded(Sender: TObject);
    procedure DoPopup(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    procedure AddJob(Job: TODJob);
    procedure Resize; override;
    function AddNewJob: TODJob;
    function AddJobFrom(AJobNo: Integer;
      const AUpperCaption, ALowerCaption: string;
      AStartDate, AFinishDate: TDate): TODJob;
    procedure InsertJob(Index: Integer; Job: TODJob);
    procedure DeleteJob(Index: Integer; ToFree: Boolean);
    function RemoveJob(Job: TODJob; ToFree: Boolean): Integer;
    procedure ClearJobs(ToFree: Boolean);
    procedure MoveJob(CurIndex, NewIndex: Integer);
    function JobAt(Index: Integer): TODJob;
    function IndexOfJob(Job: TODJob): Integer;
    function ItemByNo(AItemNo: Longint): TODJobItem;
    function JobByNo(AJobNo: Longint): TODJob;
    function ItemByCaption(const ACaption: string): TODJobItem;
    function JobByCaption(const AUpperCaption, ALowerCaption: string): TODJob;
    property JobCount: Integer read GetJobCount;
    property Jobs[Index: Integer]: TODJob read GetJob write SetJob; default;
    property ClientHeight: Integer read GetClientHeight;
    property ClientWidth: Integer read GetClientWidth;
  published
    property About: string read GetAbout write SetAbout stored False;
    property ScheduleNo: Longint read FScheduleNo write FScheduleNo default 0;
    property Header: string read GetHeader write SetHeader;
    property HeaderFont: TFont read GetHeaderFont write SetHeaderFont;
    property HeaderColor: TColor read GetHeaderColor write SetHeaderColor default clBtnFace;
    property Footer: string read GetFooter write SetFooter;
    property FooterFont: TFont read GetFooterFont write SetFooterFont;
    property FooterColor: TColor read GetFooterColor write SetFooterColor default clBtnFace;
//  property AutoAddItems: Boolean read FAutoAddItems write FAutoAddItems default True;
    property AutoScroll: Boolean read FAutoScroll write FAutoScroll default True;
    property Expanded: Boolean read FExpanded write SetExpanded default True;
    property ExpandAll: Boolean read FExpandAll write FExpandAll default True;
    property AdjustWidth: Boolean read FAdjustWidth write SetAdjustWidth default False;
    property JobWidth: Integer read FJobWidth write SetJobWidth{ stored False}default 112;
//  property MaxWidth: Integer read FMaxWidth write SetMaxWidth stored False;
    property MenuCaptions: TODScheduleMenuCaptions read FMenuCaptions write FMenuCaptions;
    property DateFormat: string read FDateFormat write FDateFormat;
    property ScrollInterval: Cardinal read GetScrollInterval write SetScrollInterval default 50;
    property Width: Integer read GetWidth write SetWidth stored False{default 118};
    property Align default alLeft;
    property Enabled;
//  property Ctl3D;
    property Font;
    property Height;
    property ParentColor;
//  property ParentCtl3D;
    property ParentFont;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnBeforeJobAdd: TODScheduleEvent read FOnBeforeJobAdd write FOnBeforeJobAdd;
    property OnBeforeJobEdit: TODJobEvent read FOnBeforeJobEdit write FOnBeforeJobEdit;
    property OnBeforeJobRemove: TODJobEvent read FOnBeforeJobRemove write FOnBeforeJobRemove;
    property OnBeforeJobFree: TODJobEvent read FOnBeforeJobFree write FOnBeforeJobFree;
    property OnAfterJobAdd: TODJobEvent read FOnAfterJobAdd write FOnAfterJobAdd;
    property OnAfterJobEdit: TODJobEvent read FOnAfterJobEdit write FOnAfterJobEdit;
    property OnAfterJobRemove: TODScheduleEvent read FOnAfterJobRemove write FOnAfterJobRemove;
    property OnAfterJobFree: TODScheduleEvent read FOnAfterJobFree write FOnAfterJobFree;
    property OnBeforeItemAdd: TODJobEvent read FOnBeforeItemAdd write FOnBeforeItemAdd;
    property OnBeforeItemEdit: TODItemEvent read FOnBeforeItemEdit write FOnBeforeItemEdit;
    property OnBeforeItemRemove: TODItemEvent read FOnBeforeItemRemove write FOnBeforeItemRemove;
    property OnBeforeItemFree: TODItemEvent read FOnBeforeItemFree write FOnBeforeItemFree;
    property OnAfterItemAdd: TODItemEvent read FOnAfterItemAdd write FOnAfterItemAdd;
    property OnAfterItemEdit: TODItemEvent read FOnAfterItemEdit write FOnAfterItemEdit;
    property OnAfterItemRemove: TODJobEvent read FOnAfterItemRemove write FOnAfterItemRemove;
    property OnAfterItemFree: TODJobEvent read FOnAfterItemFree write FOnAfterItemFree;
    property OnDoJobAdd: TODScheduleEvent read FOnDoJobAdd write FOnDoJobAdd;
    property OnDoJobEdit: TODJobEvent read FOnDoJobEdit write FOnDoJobEdit;
    property OnDoItemAdd: TODJobEvent read FOnDoItemAdd write FOnDoItemAdd;
    property OnDoItemEdit: TODItemEvent read FOnDoItemEdit write FOnDoItemEdit;
    property OnJobDragOver: TODJobDragEvent read FOnJobDragOver write FOnJobDragOver;
    property OnJobDragDrop: TODJobEvent read FOnJobDragDrop write FOnJobDragDrop;
    property OnItemDragOver: TODItemDragEvent read FOnItemDragOver write FOnItemDragOver;
    property OnItemDragDrop: TODItemEvent read FOnItemDragDrop write FOnItemDragDrop;
    property OnInitJobDialog: TODInitJobDialogEvent read FOnInitJobDialog write FOnInitJobDialog;
    property OnInitItemDialog: TODInitItemDialogEvent read FOnInitItemDialog write FOnInitItemDialog;
    property OnClick;
    property OnItemClick: TODItemEvent read FOnItemClick write FOnItemClick;
    property OnJobClick: TODJobEvent read FOnJobClick write FOnJobClick;
    property OnEnter;
    property OnExit;
    property OnDragOver;
    property OnDragDrop;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
{   property OnStartDrag;}
  end;

  TODScheduleBox = class(TScrollBox)
  private
    FSchedules: TList;
    FScrollTimer: TTimer;
    FAutoScroll, FScrollingLeft, FAdjustWidth: Boolean;
    function GetAbout: string;
    procedure SetAbout(Value: string);
    function GetScrollInterval: Cardinal;
    procedure SetScrollInterval(Value: Cardinal);
    function GetScheduleCount: Integer;
    function GetSchedule(Index: Integer): TODJobSchedule;
    procedure SetSchedule(Index: Integer; Value: TODJobSchedule);
  protected
    procedure Resize; override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); override;
    procedure EndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure ScrollTimed(Sender: TObject);
    procedure ArrangeSchedules;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure AddSchedule(ASchedule: TODJobSchedule);
    function AddNewSchedule: TODJobSchedule;
    function AddScheduleFrom(AScheduleNo: Integer;
      const AHeader, AFooter: string): TODJobSchedule;
    procedure DeleteSchedule(Index: Integer; ToFree: Boolean);
    function RemoveSchedule(Schedule: TODJobSchedule; ToFree: Boolean): Integer;
    procedure ClearSchedules(ToFree: Boolean);
    procedure MoveSchedule(CurIndex, NewIndex: Integer);
    function ScheduleAt(Index: Integer): TODJobSchedule;
    function IndexOfSchedule(Schedule: TODJobSchedule): Integer;
    function ItemByNo(AItemNo: Longint): TODJobItem;
    function JobByNo(AJobNo: Longint): TODJob;
    function ScheduleByNo(AScheduleNo: Longint): TODJobSchedule;
    function ItemByCaption(const ACaption: string): TODJobItem;
    function JobByCaption(const AUpperCaption, ALowerCaption: string): TODJob;
    function ScheduleByCaption(const AHeader, AFooter: string): TODJobSchedule;
    property ScheduleCount: Integer read GetScheduleCount;
    property Schedules[Index: Integer]: TODJobSchedule read GetSchedule write SetSchedule; default;
  published
    property About: string read GetAbout write SetAbout stored False;
    property Align default alClient;
    property AutoScroll: Boolean read FAutoScroll write FAutoScroll default True;
    property ScrollInterval: Cardinal read GetScrollInterval write SetScrollInterval default 50;
    property AdjustWidth: Boolean read FAdjustWidth write FAdjustWidth default True;
  end;

implementation

uses JobDlg, ItemDlg, System.UITypes;

const
  ODVersion = '5.0.0';
  Restricted = False;
  MaxItems = 5;
  MaxJobs = 5;
  MaxSchedules = 3;

//TODJobItem

constructor TODJobItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoSize := False;
  Left := 3;
//Width := {Job.Width - 6}105;
  Height := 15;
  Color := clWhite;
  BorderStyle := sbsSunken;
  FMenuCaptions := TODItemMenuCaptions.Create;
  PopupMenu := TPopupMenu.Create(Self);
  PopupMenu.OnPopup := DoPopup;
  FEditMenu := TMenuItem.Create(Self);
//FEditMenu.Caption := '&Edit...';
  FEditMenu.OnClick := DoItemEdit;
  PopupMenu.Items.Add(FEditMenu);
  FRemoveMenu := TMenuItem.Create(Self);
//FRemoveMenu.Caption := '&Remove';
  FRemoveMenu.OnClick := DoItemRemove;
  PopupMenu.Items.Add(FRemoveMenu);
  OnMouseMove := DoMouseMove;
  OnMouseDown := DoMouseDown;
  OnMouseUp := DoMouseUp;
  OnClick := DoClick;
  OnDblClick := {DoItemEdit}DoDblClick;
end;

destructor TODJobItem.Destroy;
begin
  FMenuCaptions.Free;
  inherited Destroy;
end;

function TODJobItem.GetJob: TODJob;
begin
  if Owner is TODJob then
    Result := TODJob(Owner) else
    Result := nil;
end;

procedure TODJobItem.DragOver(Source: TObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
begin
  if (Job <> nil) and (Source <> Self) then
    Job.DoDragOver(Self, Source, X, Y, State, Accept)
  else Accept := False;
end;

procedure TODJobItem.DragDrop(Source: TObject; X, Y: Integer);
begin
  if Job <> nil then
    Job.DoDragDrop(Self, Source, X, Y);
end;

procedure TODJobItem.DoEndDrag(Source: TObject; X, Y: Integer);
begin
  inherited DoEndDrag(Source, X, Y);
  if Job <> nil then Job.DoEndDrag(Source, X, Y);
end;

procedure TODJobItem.DoMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if (ssLeft in Shift) and not (ssDouble in Shift) then
    BeginDrag(False);
end;

procedure TODJobItem.DoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Job <> nil then
    Job.DoMouseDown(Sender, Button, Shift, X, Y);
end;

procedure TODJobItem.DoMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Job <> nil then
    Job.DoMouseUp(Sender, Button, Shift, X, Y);
end;

procedure TODJobItem.DoClick(Sender: TObject);
begin
  if Job <> nil then Job.DoClick(Sender);
end;

procedure TODJobItem.DoDblClick(Sender: TObject);
begin
  if (FMenuCaptions.Edit <> '') and  (FMenuCaptions.Edit <> ' ') then
    DoItemEdit(Sender);
end;

procedure TODJobItem.DoItemEdit(Sender: TObject);
var
  st: string;
begin
  if not (Owner is TODJob) then
    raise EComponentError.Create('Item not owned by job!');
  if not (Job.Owner is TODJobSchedule) then
    raise EComponentError.Create('Job not owned by schedule!');
  if Assigned(Job.Schedule.OnBeforeItemEdit) then
    Job.Schedule.OnBeforeItemEdit(Job.Schedule, Job, Self);
  if Assigned(Job.Schedule.OnDoItemEdit) then
  begin
    Job.Schedule.OnDoItemEdit(Job.Schedule, Job, Self);
    if Assigned(Job.Schedule.OnAfterItemEdit) then
      Job.Schedule.OnAfterItemEdit(Job.Schedule, Job, Self);
  end
  else
    if Assigned(Job.Schedule.OnInitItemDialog) then
      with TODJobItemForm.Create(Job.Schedule) do
      try
        Job := Self.Job;
        JobItem := Self;
        Caption := 'Edit Item';
        OnInitDialog := Job.Schedule.OnInitItemDialog;
        if (ShowModal = ID_OK) and Assigned(Job.Schedule.OnAfterItemEdit) then
          Job.Schedule.OnAfterItemEdit(Job.Schedule, Job, Self);
      finally
        Free;
      end
  else
  begin
    st := Caption;
    if InputQuery('Edit Job Item', 'Definition', st) then
      Caption := st;
  end;
end;

procedure TODJobItem.DoItemRemove(Sender: TObject);
begin
  Job.RemoveItem(Self, True);
end;

procedure TODJobItem.DoPopup(Sender: TObject);
begin
  with FEditMenu do
  begin
    Caption := FMenuCaptions.Edit;
    Visible := (Caption <> '') and (Caption <> ' ');
  end;
  with FRemoveMenu do
  begin
    Caption := FMenuCaptions.Remove;
    Visible := (Caption <> '') and (Caption <> ' ');
  end;
end;

//TODJob

constructor TODJob.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls];
//Width := {Schedule.Width - 21}111;   //leave room for scrollbar
  BorderWidth := 2;
  PopupMenu := TPopupMenu.Create(Self);
  PopupMenu.OnPopup := DoPopup;
  FMenuCaptions := TODJobMenuCaptions.Create;
  FAddItemMenu := TMenuItem.Create(Self);
//FAddItemsMenu.Caption := '&Add Items...';
  FAddItemMenu.OnClick := DoItemAdd;
  PopupMenu.Items.Add(FAddItemMenu);
  FClearItemsMenu := TMenuItem.Create(Self);
//FClearItemsMenu.Caption := '&Clear Items';
  FClearItemsMenu.OnClick := DoItemsClear;
  PopupMenu.Items.Add(FClearItemsMenu);
  FSeperator := TMenuItem.Create(Self);
  FSeperator.Caption := '-';
  PopupMenu.Items.Add(FSeperator);
  FEditMenu := TMenuItem.Create(Self);
//FEditMenu.Caption := '&Edit...';
  FEditMenu.OnClick := DoJobEdit;
  PopupMenu.Items.Add(FEditMenu);
  FRemoveMenu := TMenuItem.Create(Self);
//FRemoveMenu.Caption := '&Remove';
  FRemoveMenu.OnClick := DoJobRemove;
  PopupMenu.Items.Add(FRemoveMenu);
  FItems := TList.Create;
  FUpperLabel := TLabel.Create(Self);
  with FUpperLabel do
  begin
    Parent := Self;
//  Align := alTop;
    AutoSize := False;
    Top := 3;
    Left := 3;
    Width := Self.Width - {6}7;
    Height := 13;
    Alignment := taCenter;
    Font.Style := [fsBold];
    Font.Color := clWhite;
    Color := clBlue;
    OnDragOver := DoDragOver;
    OnDragDrop := DoDragDrop;
    OnEndDrag := DoEndDragging;
    OnDblClick := {DoJobEdit}DoDblClick;
    OnMouseMove := DoMouseMove;
    OnMouseDown := DoMouseDown;
    OnMouseUp := DoMouseUp;
    OnClick := DoClick;
  end;
  FLowerLabel := TLabel.Create(Self);
  with FLowerLabel do
  begin
    Parent := Self;
    Visible := False;
//  Align := alTop;
    AutoSize := False;
    Top := FUpperLabel.Height + 3;
    Left := 3;
    Width := Self.Width - {6}7;
    Height := 13;
    Alignment := taCenter;
    Font.Style := [fsBold];
    Font.Color := clWhite;
    Color := clBlue;
    OnDragOver := DoDragOver;
    OnDragDrop := DoDragDrop;
    OnEndDrag := DoEndDragging;
    OnDblClick := {DoJobEdit}DoDblClick;
    OnMouseMove := DoMouseMove;
    OnMouseDown := DoMouseDown;
    OnMouseUp := DoMouseUp;
    OnClick := DoClick;
  end;
  FDateText := TStaticText.Create(Self);
  with FDateText do
  begin
    Parent := Self;
    Visible := False;
//  Align := alTop;
    AutoSize := False;
    Top := FLowerLabel.Top + FLowerLabel.Height;
    Left := 3;
    Width := Self.Width - 6;
    Height := {17}15;
    Alignment := taCenter;
    Color := clAqua;
    BorderStyle := sbsSunken;
    OnDragOver := DoDragOver;
    OnDragDrop := DoDragDrop;
    OnEndDrag := DoEndDragging;
    OnMouseDown := DoMouseDown;
    OnMouseUp := DoMouseUp;
    OnClick := DoClick;
//  OnDblClick := DoItemAdd;
  end;
  FStatusText := TStaticText.Create(Self);
  with FStatusText do
  begin
    Parent := Self;
    Visible := False;
//  Align := alTop;
    AutoSize := False;
    Top := FDateText.Top + FDateText.Height;
    Left := 3;
    Width := Self.Width - 6;
    Height := 15;
//  Alignment := taCenter;
    Color := {clBtnFace}cl3DLight;
    Font.Color := clGray;
    BorderStyle := sbsSunken;
    OnDragOver := DoDragOver;
    OnDragDrop := DoDragDrop;
    OnEndDrag := DoEndDragging;
    OnDblClick := DoDblClick;
    OnMouseMove := DoMouseMove;
    OnMouseDown := DoMouseDown;
    OnMouseUp := DoMouseUp;
    OnClick := DoClick;
  end;
//Height := FDateText.Top + FDateText.Height + 3;
//SetDates;
  FDateFormat := {ShortDateFormat}'dd/mm/yy';
  FStartDate := Date;
  FFinishDate := Date;
  ArrangeItems;
end;

destructor TODJob.Destroy;
begin
  FItems.Free;
  FMenuCaptions.Free;
  inherited Destroy;
end;

procedure TODJob.Resize;
var
  iItem: Integer;
begin
  inherited Resize;
//ArrangeItems;
  FUpperLabel.Width := Width - 6;
  FLowerLabel.Width := Width - 6;
  FDateText.Width := Width - 6;
  FStatusText.Width := Width - 6;
  for iItem := 0 to ItemCount-1 do
    Items[iItem].Width := Width - 6;
end;

function TODJob.GetSchedule: TODJobSchedule;
begin
  if Owner is TODJobSchedule then
    Result := TODJobSchedule(Owner)
  else
    Result := nil;
end;

function TODJob.GetUpperCaption: string;
begin
  Result := FUpperLabel.Caption;
end;

procedure TODJob.SetUpperCaption(const Value: string);
begin
  FUpperLabel.Caption := Value;
end;

function TODJob.GetLowerCaption: string;
begin
  Result := FLowerLabel.Caption;
end;

procedure TODJob.SetLowerCaption(const Value: string);
begin
  with FLowerLabel do
  begin
    Visible := Value <> '';
    if (Caption <> '') <> (Value <> '') then  //visible changed
    begin
      ArrangeItems;
      if Schedule <> nil then
        Schedule.ArrangeJobs
    end;
    Caption := Value;
  end;
end;

function TODJob.GetStatus: string;
begin
  Result := FStatusText.Caption;
end;

procedure TODJob.SetStatus(const Value: string);
begin
  with FStatusText do
  begin
    Visible := (Value <> '') and FExpanded;
    if (Caption <> '') <> (Value <> '') then  //visible changed
    begin
      ArrangeItems;
      if Schedule <> nil then
        Schedule.ArrangeJobs
    end;
    Caption := Value;
  end;
end;

procedure TODJob.SetStartDate(Value: TDate);
begin
  FStartDate := Value;      //validation required
  SetDates;
end;

procedure TODJob.SetFinishDate(Value: TDate);
begin
  FFinishDate := Value;      //validation required
  SetDates;
end;

procedure TODJob.SetExpanded(Value: Boolean);
begin
  FExpanded := Value;
  FStatusText.Visible := (FStatusText.Caption <> '') and FExpanded;
  if FExpanded and FStatusText.Visible then
    Height := FStatusText.Top + FStatusText.Height + 3
  else if FExpanded and (ItemCount > 0) then
    Height := Items[ItemCount-1].Top + Items[ItemCount-1].Height + 3
  else
    Height := FDateText.Top + FDateText.Height + 3;
  if ItemCount > 0 then
    Items[0].Visible := FExpanded;
  FAddItemMenu.Enabled := FExpanded;
  FClearItemsMenu.Enabled := FExpanded;
end;

procedure TODJob.SetDates;
var
  st: string;
  vi: Boolean;
begin
//FDateText.Caption := DateToStr(FStartDate) + ' - ' + DateToStr(FFinishDate);
  if FStartDate = 0 then st := '?'
  else st := {DateToStr(FStartDate)}FormatDateTime('dd mmm', FStartDate);
  if FFinishDate = 0 then
    FDateText.Caption := {DateToStr(FStartDate)}FormatDateTime('dd mmm', FStartDate)
  else
    FDateText.Caption := st + ' - ' + {DateToStr(FFinishDate)}FormatDateTime('dd mmm', FFinishDate);
  vi := FDateText.Visible;
  FDateText.Visible := (FStartDate <> 0) or (FFinishDate <> 0);
  if FDateText.Visible <> vi then  //visible changed
  begin
    ArrangeItems;
    if Schedule <> nil then
      Schedule.ArrangeJobs
  end;
end;

procedure TODJob.SetDateFormat(const Value: string);
begin
  FDateFormat := Value;
  SetDates;
end;

function TODJob.GetFont: TFont;
begin
  Result := FUpperLabel.Font;
end;

procedure TODJob.SetFont(Value: TFont);
begin
  FUpperLabel.Font.Assign(Value);
  FLowerLabel.Font.Assign(Value);
end;

function TODJob.GetDateFont: TFont;
begin
  Result := FDateText.Font;
end;

procedure TODJob.SetDateFont(Value: TFont);
begin
  FDateText.Font.Assign(Value);
end;

function TODJob.GetStatusFont: TFont;
begin
  Result := FStatusText.Font;
end;

procedure TODJob.SetStatusFont(Value: TFont);
begin
  FStatusText.Font.Assign(Value);
end;

function TODJob.GetColor: TColor;
begin
  Result := FUpperLabel.Color;
end;

procedure TODJob.SetColor(Value: TColor);
begin
  FUpperLabel.Color := Value;
  FLowerLabel.Color := Value;
end;

function TODJob.GetDateColor: TColor;
begin
  Result := FDateText.Color;
end;

procedure TODJob.SetDateColor(Value: TColor);
begin
  FDateText.Color := Value;
end;

function TODJob.GetStatusColor: TColor;
begin
  Result := FStatusText.Color;
end;

procedure TODJob.SetStatusColor(Value: TColor);
begin
  FStatusText.Color := Value;
end;

function TODJob.GetItemCount: Integer;
begin
  Result := FItems.Count;
end;

function TODJob.GetItem(Index: Integer): TODJobItem;
begin
  Result := FItems[Index];
end;

procedure TODJob.SetItem(Index: Integer; Value: TODJobItem);
begin
  FItems[Index] := Value;
end;

procedure TODJob.DoClick(Sender: TObject);
begin
{  if Schedule <> nil then
    if Sender is TODJobItem then
      Schedule.DoClick(Sender)
    else
      Schedule.DoClick(Self);}
  if Assigned(OnClick) then
    if Sender is TODJobItem then
      OnClick(Sender)
    else
      OnClick(Self);
end;

procedure TODJob.DoDblClick(Sender: TObject);
begin
  if (FMenuCaptions.Edit <> '') and  (FMenuCaptions.Edit <> ' ') then
    DoJobEdit(Sender);
end;

procedure TODJob.DoMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if (ssLeft in Shift) and not (ssDouble in Shift) then
    BeginDrag(False);
end;

procedure TODJob.DoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Sender is TODJobItem then
    Schedule.DoMouseDown(Sender, Button, Shift, X, Y)
  else
    Schedule.DoMouseDown(Self, Button, Shift, X, Y);
end;

procedure TODJob.DoMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Sender is TODJobItem then
    Schedule.DoMouseUp(Sender, Button, Shift, X, Y)
  else
    Schedule.DoMouseUp(Self, Button, Shift, X, Y);
end;

procedure TODJob.DoDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  if Sender = Source then
    Accept := False
  else if Source is TODJobItem then
    Schedule.ItemDragOver(Self, TODJobItem(Source), FItems.IndexOf(Sender), Accept)
  else if Source is TODJob then
    if Source = Self then Accept := False
    else Schedule.JobDragOver(Self, TODJob(Source), Accept)
  else if Assigned(Schedule.OnDragOver) then
    Schedule.OnDragOver(Self, Source, X, Y, State, Accept);
end;

procedure TODJob.DoDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  if Source is TODJobItem then
    ItemDragDrop(Sender, TODJobItem(Source))
  else if Source is TODJob then
    Schedule.JobDragDrop(Self, TODJob(Source))
  else if Assigned(Schedule.OnDragDrop) then
    Schedule.OnDragDrop(Self, Source, X, Y);
  Schedule.Resize;
end;

procedure TODJob.DoEndDragging(Sender, Target: TObject; X, Y: Integer);
begin
  DoEndDrag(Target, X, Y);
end;

procedure TODJob.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  Schedule.FScrollTimer.Enabled := False;
  inherited DoEndDrag(Target, X, Y);
  Schedule.DoEndDrag(Target, X, Y);
end;

procedure TODJob.ItemDragDrop(Sender: TObject; Item: TODJobItem);
var
  je: TODJob;
  ix: Integer;
begin
  je := Item.Job;
  if Sender is TODJobItem then
    ix := FItems.IndexOf(Sender) + 1 else  //place after item dropped on
    ix := 0;           //dropped on title or date so put in front
  if Item.Job = Self then        //an internal reordering
    MoveItem(IndexOfItem(Item), ix) else
    InsertItem(ix, Item);        //dragged from another schedule
  Schedule.ItemDragDrop(Self, je, Item);
end;

procedure TODJob.AddItem(Item: TODJobItem);
begin
  if Restricted and (ItemCount >= MaxItems) then
  begin
    MessageDlg('This demo version is restricted to ' +
      IntToStr(MaxItems) + ' items per job - please refer to the ' +
      'registration information for ordering the unrestricted version.',
      mtInformation, [mbOK], 0);
    Exit;
  end;
  if (Item = nil) or (IndexOfItem(Item) > -1) then
    Exit;
  if (Item.Owner <> nil) and (Item.Owner <> Self) then
    if Item.Owner is TODJob then
      TODJob(Item.Owner).RemoveItem(Item, False)
    else
      Item.Owner.RemoveComponent(Item);
  if Item.Owner <> Self then
    InsertComponent(Item);
  Item.Parent := Self;
  if ItemCount > 0 then
    with Items[ItemCount-1] do
      Item.Top := Top + Height
  else
    Item.Top := FDateText.Top + FDateText.Height;
  Item.Width := Width - 6;
  if Schedule <> nil then
  begin
    Item.MenuCaptions.Edit := Schedule.MenuCaptions.EditItem;
    Item.MenuCaptions.Remove := Schedule.MenuCaptions.RemoveItem;
  end;
  Item.OnClick := DoClick;
  FItems.Add(Item);
  Item.Visible := True;
//Height := Height + Item.Height;
  ArrangeItems;
  if Schedule <> nil then Schedule.Resize;
  if not FAddingFrom and (Schedule <> nil) then
    Schedule.ItemAdded(Self, Item);
end;

function TODJob.AddNewItem: TODJobItem;
begin
  Result := TODJobItem.Create(Self);
  AddItem(Result);
end;

function TODJob.AddItemFrom(AItemNo: Integer; const ACaption: string): TODJobItem;
begin
  FAddingFrom := True;
  Result := AddNewItem;
  FAddingFrom := False;
  Result.ItemNo := AItemNo;
  Result.Caption := ACaption;
  if Schedule <> nil then
    Schedule.ItemAdded(Self, Result);
end;

procedure TODJob.InsertItem(Index: Integer; Item: TODJobItem);
begin
  AddItem(Item);
  MoveItem(ItemCount-1, Index);
end;

procedure TODJob.DeleteItem(Index: Integer; ToFree: Boolean);
begin
  if Schedule <> nil then
  begin
    Schedule.DoRemoveItem(Self, Items[Index]);
    if ToFree then
      Schedule.DoFreeItem(Self, Items[Index]);
  end;
  Height := Height - Items[Index].Height;
  if ToFree then
    Items[Index].Free
  else
    RemoveComponent(Items[Index]);
  FItems.Delete(Index);
  ArrangeItems;
  if Schedule <> nil then
  begin
    Schedule.Resize;
    Schedule.ItemRemoved(Self);
    if ToFree then
      Schedule.ItemFreed(Self);
  end;
end;

function TODJob.RemoveItem(Item: TODJobItem; ToFree: Boolean): Integer;
begin
  Result := FItems.IndexOf(Item);
  if Result > -1 then DeleteItem(Result, ToFree);
end;

procedure TODJob.ClearItems(ToFree: Boolean);
begin
  while FItems.Count > 0 do
    DeleteItem(0, ToFree);
end;

procedure TODJob.MoveItem(CurIndex, NewIndex: Integer);
begin
  if NewIndex > FItems.Count-1 then
    NewIndex := FItems.Count - 1;
  FItems.Move(CurIndex, NewIndex);
  ArrangeItems;
end;

function TODJob.ItemAt(Index: Integer): TODJobItem;
begin
  if Index in [0..FItems.Count-1] then
    Result := FItems[Index]
  else
    Result := nil;
end;

function TODJob.IndexOfItem(Item: TODJobItem): Integer;
begin
  Result := FItems.IndexOf(Item);
end;

function TODJob.ItemByNo(AItemNo: Longint): TODJobItem;
var
  ix: Integer;
begin
  Result := nil;
  for ix := 0 to ItemCount-1 do
    if Items[ix].ItemNo = AItemNo then
    begin
      Result := Items[ix];
      Break;
    end;
end;

function TODJob.ItemByCaption(const ACaption: string): TODJobItem;
var
  ix: Integer;
begin
  Result := nil;
  for ix := 0 to ItemCount-1 do
    if Items[ix].Caption = ACaption then
    begin
      Result := Items[ix];
      Break;
    end;
end;

procedure TODJob.ArrangeItems;
var
  ix, tp: Integer;
begin
  tp := FUpperLabel.Top + FUpperLabel.Height;
  with FLowerLabel do
    if Visible then
    begin
      Top := tp;
      tp := Top + Height;
    end;
  with FDateText do
    if Visible then
    begin
      Top := tp;
      tp := Top + Height;
    end;
  for ix := 0 to ItemCount-1 do
    with Items[ix] do
    begin
      Top := tp;
      if ix = 0 then
        Visible := FExpanded else
        Visible := True;
      if Visible then
        Inc(tp, Height);
    end;
  with FStatusText do
    if Visible then
    begin
      Top := tp;
      tp := Top + Height;
    end;
  Height := tp + 3;
end;

procedure TODJob.DoJobEdit(Sender: TObject);
begin
  if (Owner = nil) or not (Owner is TODJobSchedule) then
    raise EComponentError.Create('Job not owned by schedule!');
  if Assigned(Schedule.OnBeforeJobEdit) then
    Schedule.OnBeforeJobEdit(Schedule, Self);
  if Assigned(Schedule.OnDoJobEdit) then
  begin
    Schedule.OnDoJobEdit(Schedule, Self);    //custom editing
    if Assigned(Schedule.OnAfterJobEdit) then
      Schedule.OnAfterJobEdit(Schedule, Self);
  end
  else
    with TODJobForm.Create(Self.Schedule) do //default editing
    try
      Schedule := Self.Schedule;
      Job := Self;
      Caption := 'Edit Job';
      OnInitDialog := Schedule.OnInitJobDialog;
      if (ShowModal = ID_OK) and Assigned(Schedule.OnAfterJobEdit) then
        Schedule.OnAfterJobEdit(Schedule, Self);
    finally
      Free;
    end;
end;

procedure TODJob.DoJobRemove(Sender: TObject);
begin
  Schedule.RemoveJob(Self, True);
end;

procedure TODJob.DoItemAdd(Sender: TObject);
var
  st: string;
begin
  if (Owner = nil) or not (Owner is TODJobSchedule) then
    raise EComponentError.Create('Job not owned by schedule!');
  if Assigned(Schedule.OnBeforeItemAdd) then
    Schedule.OnBeforeItemAdd(Schedule, Self);
  if Assigned(Schedule.OnDoItemAdd) then    //custom adding -
    Schedule.OnDoItemAdd(Schedule, Self)
  else
    if Assigned(Schedule.OnInitItemDialog) then
      with TODJobItemForm.Create(Self) do    //default adding -
      try
        Job := Self;
        JobItem := nil;
        Caption := 'Add New Job Items';
        OnInitDialog := Schedule.OnInitItemDialog;
        ShowModal;
      finally
        Free;
      end
  else
    if InputQuery('Add Item', 'Definition', st) then
      AddItemFrom(0, st);
end;

procedure TODJob.DoItemsClear(Sender: TObject);
begin
  if MessageDlg('Remove all the items from this job?', mtConfirmation,
    [mbYes, mbNo, mbCancel], 0) = mrYes then ClearItems(True);
end;

procedure TODJob.DoPopup(Sender: TObject);
  procedure InitMenu(MI: TMenuItem; const MC: string);
  begin
    MI.Caption := MC;
    MI.Visible := (MC <> '') and (MC <> ' ');
  end;
begin
  InitMenu(FAddItemMenu, FMenuCaptions.AddItem);
  InitMenu(FClearItemsMenu, FMenuCaptions.ClearItems);
  InitMenu(FEditMenu, FMenuCaptions.Edit);
  InitMenu(FRemoveMenu, FMenuCaptions.Remove);
  FSeperator.Visible := (FEditMenu.Visible or FRemoveMenu.Visible) and
    (FAddItemMenu.Visible or FClearItemsMenu.Visible);
  FClearItemsMenu.Enabled := FItems.Count > 0;
end;

//TODScheduleMenuCaptions

constructor TODScheduleMenuCaptions.Create;
begin
  inherited Create;
  FAddItem := '&Add Item...';
  FEditItem := '&Edit Item...';
  FRemoveItem := '&Remove Item';
  FClearItems := '&Clear Items';
  FAddJob := '&Add New Job...';
  FEditJob := '&Edit Job...';
  FRemoveJob := '&Remove Job';
  FClearJobs := '&Clear Jobs';
  FExpanded := '&Expanded';
end;

//TODJobSchedule

constructor TODJobSchedule.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFormatSettigns:= TFormatSettings.Create;
  ControlStyle := ControlStyle - [csAcceptsControls];
  FJobs := TList.Create;
  FMenuCaptions := TODScheduleMenuCaptions.Create;
  PopupMenu := TPopupMenu.Create(Self);
  PopupMenu.OnPopup := DoPopup;
  FAddMenu := TMenuItem.Create(Self);
//FAddMenu.Caption := '&Add New Job...';
  FAddMenu.OnClick := DoJobAdd;
  PopupMenu.Items.Add(FAddMenu);
  FClearMenu := TMenuItem.Create(Self);
//FClearMenu.Caption := '&Clear Jobs';
  FClearMenu.OnClick := DoJobsClear;
  PopupMenu.Items.Add(FClearMenu);
  FSeperator := TMenuItem.Create(Self);
  FSeperator.Caption := '-';
  PopupMenu.Items.Add(FSeperator);
  FExpandMenu := TMenuItem.Create(Self);
//FExpandMenu.Caption := '&Expand Jobs';
  FExpandMenu.Checked := True;
  FExpandMenu.OnClick := DoExpanded;
  PopupMenu.Items.Add(FExpandMenu);
  FScrollTimer := TTimer.Create(Self);
  FScrollTimer.Enabled := False;
  FScrollTimer.Interval := 50;
  FScrollTimer.OnTimer := ScrollTimed;
  FAutoScroll := True;
  FHeaderLabel := TLabel.Create(Self);
  with FHeaderLabel do
  begin
    Parent := Self;
    Align := alTop;
    Alignment := taCenter;
    Font.Style := [fsBold];
//  WordWrap := True;
    Height := 0;
    OnDblClick := DoJobAdd;
    OnClick := DoClick;
    OnMouseDown := DoMouseDown;
    OnMouseUp := DoMouseUp;
    OnDragOver := DoDragOver;
    OnEndDrag := DoEndDragging;
  end;
  FFooterLabel := TLabel.Create(Self);
  with FFooterLabel do
  begin
    Parent := Self;
    Align := alBottom;
    Alignment := taCenter;
//  WordWrap := True;
    Height := 0;
    OnClick := DoClick;
    OnMouseDown := DoMouseDown;
    OnMouseUp := DoMouseUp;
    OnDragOver := DoDragOver;
    OnEndDrag := DoEndDragging;
  end;
  FScrollBox := TScrollBox.Create(Self);
  with FScrollBox do
  begin
    Parent := Self;
    Align := alClient;
    ControlStyle := ControlStyle - [csAcceptsControls];
    OnMouseMove := DoMouseMove;
    OnMouseDown := DoMouseDown;
    OnMouseUp := DoMouseUp;
    OnDragOver := DoDragOver;
    OnDragDrop := DoDragDrop;
    OnEndDrag := DoEndDragging;
    OnDblClick := {DoJobAdd}DoDblClick;
    OnClick := DoClick;
  end;
  FExpanded := True;
  FExpandAll := True;
//FAutoAddItems := True;
//FAdjustWidth := True;
//SetMaxWidth(132);
  SetJobWidth(112);
//Width := 134;
  Align := alLeft;
  FDateFormat := FormatSettings.ShortDateFormat;
  if ComponentState = [csDesigning] then  //design time instantiation
    SetHeader(Caption);           //use default name as default header
end;

destructor TODJobSchedule.Destroy;
begin
  FJobs.Free;
  FMenuCaptions.Free;
  inherited Destroy;
end;

procedure TODJobSchedule.Resize;
var
  ip: Integer;
begin
  if FResizing then Exit;
  ip := FScrollBox.VertScrollBar.Position;
  FResizing := True;
  try
    if not AdjustWidth or ((JobCount > 0) and
       (Jobs[JobCount-1].Top + Jobs[JobCount-1].Height >= {(FScrollBox.Top + FScrollBox.Height - 14)}
         FScrollBox.ClientRect.Bottom)) then
      inherited Width := FJobWidth + GetSystemMetrics(SM_CYVTHUMB) + 6
    else
      inherited Width := FJobWidth + 6;
  finally
    FResizing := False;
  end;
  inherited Resize;
  if JobCount > 0 then
    FScrollBox.VertScrollBar.Position := ip;
  if Owner is TODScheduleBox then
    TODScheduleBox(Owner).Resize;
end;

function TODJobSchedule.GetHeader: string;
begin
  Result := FHeaderLabel.Caption;
end;

procedure TODJobSchedule.SetHeader(const Value: string);
begin
  with FHeaderLabel do
  begin
    Caption := Value;
    if Caption = '' then Height := 0;
  end;
end;

function TODJobSchedule.GetHeaderFont: TFont;
begin
  Result := FHeaderLabel.Font;
end;

procedure TODJobSchedule.SetHeaderFont(Value: TFont);
begin
  FHeaderLabel.Font.Assign(Value);
end;

function TODJobSchedule.GetHeaderColor: TColor;
begin
  Result := FHeaderLabel.Color;
end;

procedure TODJobSchedule.SetHeaderColor(Value: TColor);
begin
  FHeaderLabel.Color := Value;
end;

function TODJobSchedule.GetFooter: string;
begin
  Result := FFooterLabel.Caption;
end;

procedure TODJobSchedule.SetFooter(const Value: string);
begin
  with FFooterLabel do
  begin
    Caption := Value;
    if Caption = '' then Height := 0;
  end;
end;

function TODJobSchedule.GetFooterFont: TFont;
begin
  Result := FFooterLabel.Font;
end;

procedure TODJobSchedule.SetFooterFont(Value: TFont);
begin
  FFooterLabel.Font.Assign(Value);
end;

function TODJobSchedule.GetFooterColor: TColor;
begin
  Result := FFooterLabel.Color;
end;

procedure TODJobSchedule.SetFooterColor(Value: TColor);
begin
  FFooterLabel.Color := Value;
end;

procedure TODJobSchedule.SetExpanded(Value: Boolean);
var
  ix: Integer;
begin
  FExpanded := Value;
  for ix := 0 to JobCount-1 do
    Jobs[ix].Expanded := Value;
  ArrangeJobs;
  Resize;
end;

procedure TODJobSchedule.SetAdjustWidth(Value: Boolean);
begin
  FAdjustWidth := Value;
  Resize;
end;

procedure TODJobSchedule.SetJobWidth(Value: Integer);
begin
  FJobWidth := Value;
  Resize;
end;

function TODJobSchedule.GetWidth: Integer;
begin
  Result := inherited Width;
end;

procedure TODJobSchedule.SetWidth(Value: Integer);
begin
  FJobWidth := FJobWidth + Value - inherited Width;
  inherited Width := Value;
end;

function TODJobSchedule.GetClientHeight: Integer;
begin
  Result := FScrollBox.Height - 4;
end;

function TODJobSchedule.GetClientWidth: Integer;
begin
  Result := FScrollBox.Width - 4;
end;

function TODJobSchedule.GetJobCount: Integer;
begin
  Result := FJobs.Count;
end;

function TODJobSchedule.GetJob(Index: Integer): TODJob;
begin
  Result := TODJob(FJobs[Index]);
end;

procedure TODJobSchedule.SetJob(Index: Integer; Value: TODJob);
begin
  TODJob(FJobs[Index]).Assign(Value);
end;

function TODJobSchedule.GetScrollInterval: Cardinal;
begin
  Result := FScrollTimer.Interval;
end;

procedure TODJobSchedule.SetScrollInterval(Value: Cardinal);
begin
  FScrollTimer.Interval := Value;
end;

procedure TODJobSchedule.DoClick(Sender: TObject);
begin
  if (Sender is TODJobItem) and Assigned(FOnItemClick) then
    FOnItemClick(Self, TODJobItem(Sender).Job, TODJobItem(Sender))
  else if (Sender is TODJob) and Assigned(FOnJobClick) then
    FOnJobClick(Self, TODJob(Sender))
  else if Assigned(OnClick) then
    OnClick(Self);
end;

procedure TODJobSchedule.DoDblClick(Sender: TObject);
begin
  if (FMenuCaptions.AddJob <> '') and (FMenuCaptions.AddJob <> ' ') then
    DoJobAdd(Sender);
end;

procedure TODJobSchedule.DoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(OnMouseDown) then
    if (Sender is TODJobItem) or (Sender is TODJob) then
      OnMouseDown(Sender, Button, Shift, X, Y)
    else
      OnMouseDown(Self, Button, Shift, X, Y);
end;

procedure TODJobSchedule.DoMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(OnMouseUp) then
    if (Sender is TODJobItem) or (Sender is TODJob) then
      OnMouseUp(Sender, Button, Shift, X, Y)
    else
      OnMouseUp(Self, Button, Shift, X, Y);
{}if Owner is TODScheduleBox then
    TODScheduleBox(Owner).FScrollTimer.Enabled := False;
end;

procedure TODJobSchedule.DoMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if (ssLeft in Shift) and not (ssDouble in Shift) then
    BeginDrag(False);
end;

procedure TODJobSchedule.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  if Source is TODJobItem then
    Accept := False
  else if Source is TODJob then
    JobDragOver(nil, TODJob(Source), Accept)
  else
    inherited DragOver(Source, X, Y, State, Accept);
end;

procedure TODJobSchedule.DragDrop(Source: TObject; X, Y: Integer);
begin
  if Source is TODJob then
    JobDragDrop(nil, TODJob(Source))
  else
    inherited DragDrop(Source, X, Y);
end;

procedure TODJobSchedule.ScrollTimed(Sender: TObject);
var
  ip: Integer;
begin
  with FScrollBox.VertScrollBar do
  begin
    ip := Position;
    if FScrollingUp then
      Position := Position - Increment
    else
      Position := Position + Increment;
//    if (Position = 0) or (Position = Range) then
    if Position = ip then       //no change
      FScrollTimer.Enabled := False;
  end;
end;

procedure TODJobSchedule.DoDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  if (Sender = FScrollBox) and
     ((Source is TODJob) or (Source is TODJobItem)) then
  begin
    FScrollingUp := Y < 5;
    FScrollTimer.Enabled := (Y < 5) or (Y > FScrollBox.Height-7);
  end
  else if (Sender = FHeaderLabel) or (Sender = FFooterLabel) then
  begin
    FScrollingUp := Sender = FHeaderLabel;
    FScrollTimer.Enabled := True;
  end;
  if Source is TODJobItem then
    Accept := False
  else if Source is TODJob then
    JobDragOver(nil, TODJob(Source), Accept)
  else if Assigned(OnDragOver) then
    OnDragOver(Self, Source, X, Y, State, Accept);
end;

procedure TODJobSchedule.DoDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  FScrollTimer.Enabled := False;
  if Source is TODJob then
    JobDragDrop(Self, TODJob(Source))
  else if Assigned(OnDragDrop) then
    OnDragDrop(Self, Source, X, Y);
end;

procedure TODJobSchedule.DoEndDragging(Sender, Target: TObject; X, Y: Integer);
begin
  DoEndDrag(Target, X, Y);
end;

procedure TODJobSchedule.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  FScrollTimer.Enabled := False;
  if Owner is TODScheduleBox then
    TODScheduleBox(Owner).FScrollTimer.Enabled := False;
  inherited DoEndDrag(Target, X, Y);
end;

procedure TODJobSchedule.JobDragOver(Sender, Job: TODJob; var Accept: Boolean);
begin
  if Assigned(FOnJobDragOver) then
    FOnJobDragOver(Self, Job, FJobs.IndexOf(Sender), Accept);
end;

procedure TODJobSchedule.ItemDragOver(Sender: TODJob; Item: TODJobItem;
  Index: Integer; var Accept: Boolean);
begin
  if Assigned(FOnItemDragOver) then
    FOnItemDragOver(Self, Sender, Item, Index, Accept);
end;

procedure TODJobSchedule.JobDragDrop(Sender: TObject; Job: TODJob);
var
  ix: Integer;
begin
  FScrollTimer.Enabled := False;
  if Owner is TODScheduleBox then
    TODScheduleBox(Owner).FScrollTimer.Enabled := False;
  if Sender is TODJob then
    ix := FJobs.IndexOf(Sender)     //insert before item dropped on
  else
    ix := FJobs.Count;                 //place last
  if Job.Owner = Self then
    MoveJob(IndexOfJob(Job), ix)     //an internal reordering
  else
  begin
    InsertJob(ix, Job);               //dragged from another schedule
    Job.Width := FJobWidth;
  end;
  Resize;
  if Assigned(FOnJobDragDrop) then
    FOnJobDragDrop(Self, Job);
end;

procedure TODJobSchedule.ItemDragDrop(Sender: TObject; Job: TODJob;
  Item: TODJobItem);
begin
  FScrollTimer.Enabled := False;
  if Owner is TODScheduleBox then
    TODScheduleBox(Owner).FScrollTimer.Enabled := False;
  if Assigned(FOnItemDragDrop) then
    FOnItemDragDrop(Self, Job, Item);
end;

procedure TODJobSchedule.ItemAdded(Job: TODJob; Item: TODJobItem);
begin
  ArrangeJobs;          //adjust for the extra height
  Resize;
  FScrollBox.VertScrollBar.Position := Job.Top;
  if Assigned(FOnAfterItemAdd) then
    FOnAfterItemAdd(Self, Job, Item);
end;

procedure TODJobSchedule.ItemRemoved(Job: TODJob);
begin
  ArrangeJobs;       //adjust for the reduced height
  Resize;
  FScrollBox.VertScrollBar.Position := Job.Top;
  if Assigned(FOnAfterItemRemove) then
    FOnAfterItemRemove(Self, Job);
end;

procedure TODJobSchedule.ItemFreed(Job: TODJob);
begin
  if Assigned(FOnAfterItemFree) then
    FOnAfterItemFree(Self, Job);
end;

procedure TODJobSchedule.AddJob(Job: TODJob);
begin
  FScrollTimer.Enabled := False;
  if Restricted and (JobCount >= MaxJobs) then
  begin
    MessageDlg('This demo version is restricted to ' +
      IntToStr(MaxJobs) + ' jobs per schedule - please refer to the ' +
      'registration information for ordering the unrestricted version.',
      mtInformation, [mbOK], 0);
    Exit;
  end;
  if (Job = nil) or (IndexOfJob(Job) > -1) then
    Exit;
  if (Job.Owner <> nil) and (Job.Owner <> Self) then
    if Job.Owner is TODJobSchedule then
      Job.Schedule.RemoveJob(Job, False)
    else
      Job.Owner.RemoveComponent(Job);
  if Job.Owner <> Self then
    InsertComponent(Job);
  Job.Parent := FScrollBox;
  if JobCount > 0 then
    with Jobs[JobCount-1] do
      Job.Top := Top + Height;
//Job.Width := Width - GetSystemMetrics(SM_CYVTHUMB) - 6; //allow for scrollbar
  Job.Width := FJobWidth;
  Job.Expanded := FExpanded;
  with Job.MenuCaptions do
  begin
    AddItem := MenuCaptions.AddItem;
    ClearItems := MenuCaptions.ClearItems;
    Edit := MenuCaptions.EditJob;
    Remove := MenuCaptions.RemoveJob;
  end;
  Job.OnClick := DoClick;
  FJobs.Add(Job);
  if not FAddingFrom and Assigned(FOnAfterJobAdd) then
    FOnAfterJobAdd(Self, Job);
  Resize;
  FScrollBox.VertScrollBar.Position := Job.Top;
end;

function TODJobSchedule.AddNewJob: TODJob;
begin
  Result := TODJob.Create(Self);
  AddJob(Result);
end;

function TODJobSchedule.AddJobFrom(AJobNo: Longint;
  const AUpperCaption, ALowerCaption: string;
  AStartDate, AFinishDate: TDate): TODJob;
begin
  FAddingFrom := True;   //prevent OnJobAdded from being fired here
  Result := AddNewJob;
  FAddingFrom := False;
  with Result do
  begin
    JobNo := AJobNo;
    UpperCaption := AUpperCaption;
    LowerCaption := ALowerCaption;
    StartDate := AStartDate;
    FinishDate := AFinishDate;
  end;
  if Assigned(FOnAfterJobAdd) then
    FOnAfterJobAdd(Self, Result);
end;

procedure TODJobSchedule.InsertJob(Index: Integer; Job: TODJob);
begin
  FAddingFrom := True;
  AddJob(Job);
  FAddingFrom := False;
  MoveJob(JobCount-1, Index);
  if Assigned(FOnAfterJobAdd) then
    FOnAfterJobAdd(Self, Job);
end;

procedure TODJobSchedule.DeleteJob(Index: Integer; ToFree: Boolean);
begin
  if Assigned(FOnBeforeJobRemove) then
    FOnBeforeJobRemove(Self, Jobs[Index]);
  if ToFree and Assigned(FOnBeforeJobFree) then
    FOnBeforeJobFree(Self, Jobs[Index]);
  if ToFree then Jobs[Index].Free
  else RemoveComponent(Jobs[Index]);
  FJobs.Delete(Index);
  ArrangeJobs;
  Resize;
  FScrollTimer.Enabled := False;
  if Assigned(FOnAfterJobRemove) then
    FOnAfterJobRemove(Self);
  if ToFree and Assigned(FOnAfterJobFree) then
    FOnAfterJobFree(Self);
end;

function TODJobSchedule.RemoveJob(Job: TODJob; ToFree: Boolean): Integer;
begin
  Result := FJobs.IndexOf(Job);
  if Result > -1 then DeleteJob(Result, ToFree);
end;

procedure TODJobSchedule.ClearJobs(ToFree: Boolean);
begin
  while FJobs.Count > 0 do
    DeleteJob(0, ToFree);
end;

procedure TODJobSchedule.MoveJob(CurIndex, NewIndex: Integer);
begin
  FScrollTimer.Enabled := False;
  if NewIndex > FJobs.Count-1 then
    NewIndex := FJobs.Count - 1;
  FJobs.Move(CurIndex, NewIndex);
  ArrangeJobs;
  FScrollBox.VertScrollBar.Position := Jobs[NewIndex].Top;
end;

function TODJobSchedule.JobAt(Index: Integer): TODJob;
begin
  if Index in [0..FJobs.Count-1] then
    Result := FJobs[Index]
  else
    Result := nil;
end;

function TODJobSchedule.IndexOfJob(Job: TODJob): Integer;
begin
  Result := FJobs.IndexOf(Job);
end;

function TODJobSchedule.ItemByNo(AItemNo: Longint): TODJobItem;
var
  ix: Integer;
begin
  Result := nil;
  for ix := 0 to JobCount-1 do
  begin
    Result := Jobs[ix].ItemByNo(AItemNo);
    if Result <> nil then Break;
  end;
end;

function TODJobSchedule.JobByNo(AJobNo: Longint): TODJob;
var
  ix: Integer;
begin
  Result := nil;
  for ix := 0 to JobCount-1 do
    if Jobs[ix].JobNo = AJobNo then
    begin
      Result := Jobs[ix];
      Break;
    end;
end;

function TODJobSchedule.ItemByCaption(const ACaption: string): TODJobItem;
var
  ix: Integer;
begin
  Result := nil;
  for ix := 0 to JobCount-1 do
  begin
    Result := Jobs[ix].ItemByCaption(ACaption);
    if Result <> nil then Break;
  end;
end;

function TODJobSchedule.JobByCaption(
  const AUpperCaption, ALowerCaption: string): TODJob;
var
  ix: Integer;
begin
  Result := nil;
  for ix := 0 to JobCount-1 do
    if ((AUpperCaption = '') or (Jobs[ix].UpperCaption = AUpperCaption)) and
       ((ALowerCaption = '') or (Jobs[ix].LowerCaption = ALowerCaption)) then
    begin
      Result := Jobs[ix];
      Break;
    end;
end;

procedure TODJobSchedule.ArrangeJobs;
var
  ix, tp: Integer;
begin
//tp := 0 - FScrollBox.VertScrollBar.Position;
  FScrollBox.VertScrollBar.Position := 0;
  tp := 0;
  for ix := 0 to JobCount-1 do
  begin
    Jobs[ix].Top := tp;
    Inc(tp, Jobs[ix].Height);
  end;
end;

procedure TODJobSchedule.DoJobAdd(Sender: TObject);
begin
  if Assigned(FOnBeforeJobAdd) then
    FOnBeforeJobAdd(Self);
  if Assigned(FOnDoJobAdd) then
    FOnDoJobAdd(Self)             //custom adding used
  else
    with TODJobForm.Create(Self) do  //do default adding
    try
      Caption := 'Add New Job';
      Schedule := Self;
      Job := nil;
      OnInitDialog := OnInitJobDialog;
      OnInitItemDialog := Self.OnInitItemDialog;
      ShowModal;
    finally
      Free;
    end;
{ if FAutoAddItems and (JobCount > 0) then
    Jobs[JobCount-1].DoItemAdd(Self);}
end;

procedure TODJobSchedule.DoJobsClear(Sender: TObject);
begin
  if MessageDlg('Remove all the jobs from this schedule?', mtConfirmation,
    [mbYes, mbNo, mbCancel], 0) = mrYes then ClearJobs(True);
end;

procedure TODJobSchedule.DoRemoveItem(Sender: TObject; Item: TODJobItem);
begin
  FScrollTimer.Enabled := False;
  if Assigned(FOnBeforeItemRemove) then
    FOnBeforeItemRemove(Self, Sender as TODJob, Item);
end;

procedure TODJobSchedule.DoFreeItem(Sender: TObject; Item: TODJobItem);
begin
  if Assigned(FOnBeforeItemFree) then
    FOnBeforeItemFree(Self, Sender as TODJob, Item);
end;

procedure TODJobSchedule.DoExpanded(Sender: TObject);
var
  ix: Integer;
begin
  with Sender as TMenuItem do
  begin
    if Owner = Self then
    begin
      Checked := not Checked;         //toggle
      if FExpandAll then     //make other schedules do the same -
        with Self.Parent do
          for ix := 0 to ControlCount-1 do
            if (Controls[ix] is TODJobSchedule) and (Controls[ix] <> Self) then
              TODJobSchedule(Controls[ix]).SetExpanded(Checked);
    end;
    SetExpanded(Checked);
  end;
end;

procedure TODJobSchedule.DoPopup(Sender: TObject);
  procedure SetupMenuItem(MI: TMenuItem; const MC: string);
  begin
    MI.Caption := MC;
    MI.Visible := (MC <> '') and (MC <> ' ');
  end;
begin
  SetupMenuItem(FAddMenu, FMenuCaptions.AddJob);
  SetupMenuItem(FClearMenu, FMenuCaptions.ClearJobs);
  SetupMenuItem(FExpandMenu, FMenuCaptions.Expanded);
  FSeperator.Visible := (FAddMenu.Visible or FClearMenu.Visible) and
    FExpandMenu.Visible;
  FClearMenu.Enabled := FJobs.Count > 0;
  FExpandMenu.Checked := FExpanded;
end;

function TODJobSchedule.GetAbout: string;
begin
  Result := 'Version ' + ODVersion;
  if Restricted then
    Result := Result + ' (Demo)';
end;

procedure TODJobSchedule.SetAbout(Value: string);
begin
  {do nothing}
end;

//TODScheduleBox

constructor TODScheduleBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls];
  FSchedules := TList.Create;
  FScrollTimer := TTimer.Create(Self);
  FScrollTimer.Enabled := False;
  FScrollTimer.Interval := 50;
  FScrollTimer.OnTimer := ScrollTimed;
  FAutoScroll := True;
  FAdjustWidth := True;
  Align := alClient;
end;

destructor TODScheduleBox.Destroy;
begin
  inherited Destroy;
  FSchedules.Free;
end;

procedure TODScheduleBox.Loaded;
var
  ix: Integer;
begin
  inherited Loaded;
  for ix := 0 to ControlCount-1 do
    if Controls[ix] is TODJobSchedule then
      FSchedules.Add(Controls[ix]);
  ArrangeSchedules;
end;

procedure TODScheduleBox.AddSchedule(ASchedule: TODJobSchedule);
begin
  if Restricted and (ScheduleCount >= MaxSchedules) then
    raise EComponentError.Create('This demo version is restricted to ' +
      IntToStr(MaxSchedules) + ' schedules - please refer to the ' +
      'registration information for ordering the unrestricted version.');
  ASchedule.Parent := Self;
  ASchedule.Align := alNone;
  ASchedule.AdjustWidth := FAdjustWidth;
  FSchedules.Add(ASchedule);
  ArrangeSchedules;
end;

function TODScheduleBox.AddNewSchedule: TODJobSchedule;
begin
  Result := TODJobSchedule.Create(Self);
  AddSchedule(Result);
end;

function TODScheduleBox.AddScheduleFrom(AScheduleNo: Longint;
  const AHeader, AFooter: string): TODJobSchedule;
begin
  Result := AddNewSchedule;
  Result.ScheduleNo := AScheduleNo;
  Result.Header := AHeader;
  Result.Footer := AFooter;
end;

procedure TODScheduleBox.DeleteSchedule(Index: Integer; ToFree: Boolean);
begin
  if ToFree then Schedules[Index].Free
  else RemoveComponent(Schedules[Index]);
  FSchedules.Delete(Index);
  ArrangeSchedules;
end;

function TODScheduleBox.RemoveSchedule(Schedule: TODJobSchedule;
  ToFree: Boolean): Integer;
begin
  Result := FSchedules.IndexOf(Schedule);
  if Result > -1 then DeleteSchedule(Result, ToFree);
end;

procedure TODScheduleBox.ClearSchedules(ToFree: Boolean);
begin
  while FSchedules.Count > 0 do
    DeleteSchedule(0, ToFree);
end;

procedure TODScheduleBox.MoveSchedule(CurIndex, NewIndex: Integer);
begin
  if NewIndex > FSchedules.Count-1 then
    NewIndex := FSchedules.Count - 1;
  FSchedules.Move(CurIndex, NewIndex);
  ArrangeSchedules;
end;

function TODScheduleBox.ScheduleAt(Index: Integer): TODJobSchedule;
begin
  if Index in [0..FSchedules.Count-1] then
    Result := FSchedules[Index]
  else
    Result := nil;
end;

function TODScheduleBox.IndexOfSchedule(Schedule: TODJobSchedule): Integer;
begin
  Result := FSchedules.IndexOf(Schedule);
end;

function TODScheduleBox.ItemByNo(AItemNo: Integer): TODJobItem;
var
  ix, iy: Integer;
begin
  Result := nil;
  for ix := 0 to ScheduleCount-1 do
    for iy := 0 to Schedules[ix].JobCount-1 do begin
      Result := Schedules[ix][iy].ItemByNo(AItemNo);
      if Result <> nil then Break;
    end;
end;

function TODScheduleBox.JobByNo(AJobNo: Longint): TODJob;
var
  ix: Integer;
begin
  Result := nil;
  for ix := 0 to ScheduleCount-1 do begin
    Result := Schedules[ix].JobByNo(AJobNo);
    if Result <> nil then Break;
  end;
end;

function TODScheduleBox.ScheduleByNo(AScheduleNo: Integer): TODJobSchedule;
var
  ix: Integer;
begin
  Result := nil;
  for ix := 0 to ScheduleCount-1 do
    if AScheduleNo = Schedules[ix].ScheduleNo then
    begin
      Result := Schedules[ix];
      Break;
    end;
end;

function TODScheduleBox.ItemByCaption(const ACaption: string): TODJobItem;
var
  ix, iy: Integer;
begin
  Result := nil;
  for ix := 0 to ScheduleCount-1 do
    for iy := 0 to Schedules[ix].JobCount-1 do
    begin
      Result := Schedules[ix][iy].ItemByCaption(ACaption);
      if Result <> nil then Break;
    end;
end;

function TODScheduleBox.JobByCaption(
  const AUpperCaption, ALowerCaption: string): TODJob;
var
  ix: Integer;
begin
  Result := nil;
  for ix := 0 to ScheduleCount-1 do
  begin
    Result := Schedules[ix].JobByCaption(AUpperCaption, ALowerCaption);
    if Result <> nil then Break;
  end;
end;

function TODScheduleBox.ScheduleByCaption(
  const AHeader, AFooter: string): TODJobSchedule;
var
  ix: Integer;
begin
  Result := nil;
  for ix := 0 to ScheduleCount-1 do
    if ((AHeader = '') or (Schedules[ix].Header = AHeader)) and
       ((AFooter = '') or (Schedules[ix].Footer = AFooter)) then
    begin
      Result := Schedules[ix];
      Break;
    end;
end;

procedure TODScheduleBox.ArrangeSchedules;
var
  ix, lp: Integer;
begin
  HorzScrollBar.Position := 0;
  lp := 0;
  for ix := 0 to ScheduleCount-1 do
  begin
    Schedules[ix].Left := lp;
    Inc(lp, Schedules[ix].Width);
  end;
end;

procedure TODScheduleBox.Resize;
var
  iSched, aHeight, aLeft: Integer;
begin
  inherited Resize;
  if ScheduleCount = 0 then Exit;
//HorzScrollBar.Position := 0;
  aHeight := {ClientHeight} Height - 4;
  with Schedules[ScheduleCount-1] do
    if Left + Width > {Self.ClientWidth} Self.Width - 4 then
      aHeight := aHeight - GetSystemMetrics(SM_CXHTHUMB);  //adjust for horz scrollbar
  aLeft := HorzScrollBar.Position * -1;
  for iSched := 0 to ScheduleCount-1 do
    with Schedules[iSched] do
    begin
      Left := aLeft;
      Inc(aLeft, Width);
      Height := aHeight;
    end;
end;

procedure TODScheduleBox.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  inherited DragOver(Source, X, Y, State, Accept);
  if (Source is TODJob) or (Source is TODJobItem) then
  begin
    FScrollingLeft := X < 5;
    FScrollTimer.Enabled := (X < 5) or (X > Width - 7);
  end;
end;

procedure TODScheduleBox.EndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  FScrollTimer.Enabled := False;
end;

procedure TODScheduleBox.ScrollTimed(Sender: TObject);
var
  ip: Integer;
begin
  with HorzScrollBar do
  begin
    ip := Position;
    if FScrollingLeft then
      Position := Position - Increment
    else
      Position := Position + Increment;
//    if (Position = 0) or (Position = Range) then
    if Position = ip then
      FScrollTimer.Enabled := False;
  end;
end;

function TODScheduleBox.GetScrollInterval: Cardinal;
begin
  Result := FScrollTimer.Interval;
end;

procedure TODScheduleBox.SetScrollInterval(Value: Cardinal);
begin
  FScrollTimer.Interval := Value;
end;

function TODScheduleBox.GetScheduleCount: Integer;
begin
  Result := FSchedules.Count;
end;

function TODScheduleBox.GetSchedule(Index: Integer): TODJobSchedule;
begin
  Result := TODJobSchedule(FSchedules[Index]);
end;

procedure TODScheduleBox.SetSchedule(Index: Integer; Value: TODJobSchedule);
begin
  TODJobSchedule(FSchedules[Index]).Assign(Value);
end;

function TODScheduleBox.GetAbout: string;
begin
  Result := 'Version ' + ODVersion;
  if Restricted then
    Result := Result + ' (Demo)';
end;

procedure TODScheduleBox.SetAbout(Value: string);
begin
  {do nothing}
end;

end.
