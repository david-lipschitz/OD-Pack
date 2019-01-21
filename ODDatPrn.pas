unit ODDatPrn;
{
  TODDataPrinter Component
  My Power Station Technology (Pty) Ltd - was Orbital Decisions
  P.O.Box 1080, Milnerton 7435, South Africa
  components@mypowerstation.biz
  http://www.orbital.co.za/text/prodlist.htm
  Copyright (c) 1998-2019

  Use at your own risk!
}

interface

uses
  System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Dialogs,
  Data.DB, Vcl.Printers;

type
  TODRowPrintEvent = procedure(Sender: TObject; var RowTop: Integer) of object;
  TODColPrintEvent = procedure(Sender: TObject; ColIndex: Integer) of object;
  TODRowPrintQueryEvent = procedure(Sender: TObject; var CanPrint: Boolean) of object;
//TODColPrintQueryEvent = procedure(Sender: TObject; ColIndex: Integer; var CanPrint: Boolean) of object;

  TODDataPrinter = class(TComponent)
  private
    FDataSet: TDataSet;
    FAutoOpen, FAutoClose, FDoColHeaders, FDoTimeStamp,
      FDoPageNumbers, FDoPrintDialog: Boolean;
    FMargin, FSpacing: Smallint;
    FTitle, FStampFormat: string;
    FSubTitles: TStrings;
    FTitleFont, FSubTitleFont, FHeaderFont, FDataFont: TFont;
    FBeforeStartPrint, FAfterFinishPrint,
      FBeforeRowPrint, FAfterRowPrint: TODRowPrintEvent;
    FBeforeColPrint, FAfterColPrint: TODColPrintEvent;
    FOnRowPrintQuery: TODRowPrintQueryEvent;
//  FOnColPrintQuery: TODColPrintQueryEvent;
    FRowTop: Integer;
    function GetAbout: string;
    procedure SetAbout(Value: string);
    procedure SetDataSet(Value: TDataSet);
    procedure SetSubTitles(Value: TStrings);
    procedure SetTitleFont(Value: TFont);
    procedure SetSubTitleFont(Value: TFont);
    procedure SetHeaderFont(Value: TFont);
    procedure SetDataFont(Value: TFont);
  protected
    procedure DoPrint;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function Print: Boolean;
  published
    property About: string read GetAbout write SetAbout stored False;
    property DataSet: TDataSet read FDataSet write SetDataSet;
    property Title: string read FTitle write FTitle;
    property SubTitles: TStrings read FSubTitles write SetSubTitles;
    property StampFormat: string read FStampFormat write FStampFormat;
    property DoTimeStamp: Boolean read FDoTimeStamp write FDoTimeStamp default True;
    property DoPageNumbers: Boolean read FDoPageNumbers write FDoPageNumbers default True;
    property DoPrintDialog: Boolean read FDoPrintDialog write FDoPrintDialog default True;
    property DoColHeaders: Boolean read FDoColHeaders write FDoColHeaders default True;
    property Margin: Smallint read FMargin write FMargin default 50;
    property Spacing: Smallint read FSpacing write FSpacing default 8;
    property TitleFont: TFont read FTitleFont write SetTitleFont;
    property SubTitleFont: TFont read FSubTitleFont write SetSubTitleFont;
    property HeaderFont: TFont read FHeaderFont write SetHeaderFont;
    property DataFont: TFont read FDataFont write SetDataFont;
    property AutoOpen: Boolean read FAutoOpen write FAutoOpen default True;
    property AutoClose: Boolean read FAutoClose write FAutoClose default True;
    property BeforeStartPrint: TODRowPrintEvent read FBeforeStartPrint write FBeforeStartPrint;
    property AfterFinishPrint: TODRowPrintEvent read FAfterFinishPrint write FAfterFinishPrint;
    property BeforeRowPrint: TODRowPrintEvent read FBeforeRowPrint write FBeforeRowPrint;
    property AfterRowPrint: TODRowPrintEvent read FAfterRowPrint write FAfterRowPrint;
    property BeforeColPrint: TODColPrintEvent read FBeforeColPrint write FBeforeColPrint;
    property AfterColPrint: TODColPrintEvent read FAfterColPrint write FAfterColPrint;
    property OnRowPrintQuery: TODRowPrintQueryEvent read FOnRowPrintQuery write FOnRowPrintQuery;
//  property OnColPrintQuery: TODColPrintQueryEvent read FOnColPrintQuery write FOnColPrintQuery;
  end;


implementation

type
  TODColInfo = array[0..63] of Integer;

const
  EvaluationVersion = False;
  ODVersion = '2.1.3';


constructor TODDataPrinter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSubTitles := TStringList.Create;
  FTitleFont := TFont.Create;
  FTitleFont.Name := 'Arial';
  FTitleFont.Size := 12;
  FTitleFont.Style := [fsBold];
  FSubTitleFont := TFont.Create;
  FSubTitleFont.Name := FTitleFont.Name;
  FSubTitleFont.Size := 10;
  FHeaderFont := TFont.Create;
  FHeaderFont.Name := 'Courier New';
  FHeaderFont.Size := 10;
  FHeaderFont.Style := [fsUnderline];
  FDataFont := TFont.Create;
  FDataFont.Name := FHeaderFont.Name;
  FDataFont.Size := 10;
  FDoTimeStamp := True;
  FDoPageNumbers := True;
  FDoPrintDialog := True;
  FDoColHeaders := True;
  FAutoOpen := True;
  FAutoClose := True;
  FMargin := 50;
  FSpacing := 8;
  FStampFormat := 'dd/mm/yyyy hh:mm am/pm';
end;

destructor TODDataPrinter.Destroy;
begin
  FSubTitles.Free;
  FTitleFont.Free;
  FSubTitleFont.Free;
  FHeaderFont.Free;
  FDataFont.Free;
  inherited Destroy;
end;

procedure TODDataPrinter.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FDataSet) then
    FDataSet := nil;
end;

function TODDataPrinter.Print: Boolean;
begin
  if (FDataSet = nil) or not FDataSet.Active then
    raise EComponentError.Create('Inaccessable dataset');
  Result := True;
  if FDoPrintDialog then
    with TPrintDialog.Create(Self) do
    try
      Result := Execute;
    finally
      Free;
    end;
  if Result then DoPrint;
end;

procedure TODDataPrinter.DoPrint;
var
  ColPixLefts, ColCharWidths: TODColInfo;
  ix, iLeft, CharHeight, CharWidth: Integer;
  OutStr: string;
  Bkmk: TBookmark;
  CanPrint: Boolean;

  procedure PrintColHeaders;
  var
    ix, ip: Integer;
    al, st: string;
  begin
    with Printer.Canvas do
    begin
      Font.Assign(FHeaderFont);
      for ix := 0 to FDataSet.FieldCount-1 do
        with FDataSet.Fields[ix] do
          if Visible then
          begin
            if FDataSet.Fields[ix] is TNumericField then
              al := ''     //right align
            else
              al := '-';    //left align
            st := DisplayLabel;
            for ip := DisplayWidth to ColCharWidths[ix] do
              if FDataSet.Fields[ix] is TNumericField then
                st := '_' + st
              else
                st := st + '_';
            TextOut(ColPixLefts[ix], FRowTop, Format('%' + al + '*.*s',
              [ColCharWidths[ix], ColCharWidths[ix], DisplayLabel]));
          end;
    end;
  end;

begin
  FRowTop := 0;
  if FAutoOpen then Printer.BeginDoc;
  if Assigned(FBeforeStartPrint) then
    FBeforeStartPrint(Self, FRowTop);
  with Printer, Canvas do
  try
    //get column lefts and widths -----------------------------
    Font.Assign(FDataFont);
    CharWidth := TextWidth('a');
    iLeft := FMargin;
    for ix := 0 to FDataSet.FieldCount-1 do   //get col widths in chars
      with FDataSet.Fields[ix] do
        if Visible then
        begin
          if TextWidth(DisplayLabel) div CharWidth > DisplayWidth then
            ColCharWidths[ix] := TextWidth(DisplayLabel) div CharWidth
          else
            ColCharWidths[ix] := DisplayWidth;
          ColPixLefts[ix] := iLeft;
          Inc(iLeft, (ColCharWidths[ix] * CharWidth) + FSpacing);
        end;
     FRowTop := 0;
    //print evaluation notice--------------------------------
    if EvaluationVersion then
    begin
      Font.Assign(FDataFont);
      Font.Style := [fsBold];
      TextOut(FMargin, FRowTop, 'Printed by an evaluation version of TODDataPrinter');
      Inc(FRowTop, TextHeight('A'));
      TextOut(FMargin, FRowTop,' Copyright (c) 1998 by Orbital Decisions');
      Inc(FRowTop, TextHeight('A') * 2);
    end;
    //print title --------------------------------------------
    Font.Assign(FTitleFont);
    if FTitle <> '' then
      TextOut(FMargin, FRowTop, FTitle);
    Font.Assign(FSubTitleFont);
    if FDoTimeStamp and (FStampFormat <> '') then
      TextOut(PageWidth - Round(PageWidth/2.5), FRowTop,
        {'Printed: ' +} FormatDateTime(FStampFormat, Now));
    if FDoPageNumbers then
    begin
      OutStr := 'Page ' + IntToStr(PageNumber);
      TextOut(PageWidth - TextWidth(OutStr) - 50, FRowTop, OutStr);
    end;
    if (FTitle <> '') or FDoTimeStamp or FDoPageNumbers then
    begin
      Font.Assign(FTitleFont);
      Inc(FRowTop, TextHeight('A'));
    end;
    //print script -------------------------------------------
    if FSubTitles.Count > 0 then
    begin
      Font.Assign(FSubTitleFont);
      CharHeight := TextHeight('A');
      for ix := 0 to FSubTitles.Count-1 do
      begin
        TextOut(FMargin, FRowTop, FSubTitles[ix]);
        Inc(FRowTop, CharHeight);
      end;
      Inc(FRowTop, CharHeight div 2);
    end;
    //print headers & body ---------------------------------------
    if FDoColHeaders then PrintColHeaders;
    Font.Assign(FDataFont);
    CharHeight := TextHeight('A');
    Inc(FRowTop, CharHeight + (CharHeight div 3));
    with FDataSet do
    begin
      Bkmk := GetBookmark;
      DisableControls;
      try
        First;
        while not EOF do
        begin
          if FRowTop + CharHeight > PageHeight then
          begin
            NewPage;
            FRowTop := 0;
            if FDoPageNumbers then
            begin
              OutStr := 'Page ' + IntToStr(PageNumber);
              TextOut(PageWidth - TextWidth(OutStr) - 50, FRowTop, OutStr);
              Inc(FRowTop, TextHeight('A') * 2);
            end;
            if FDoColHeaders then PrintColHeaders;
            Font.Assign(FDataFont);
            CharHeight := TextHeight('A');
            Inc(FRowTop, CharHeight + (CharHeight div 3));
          end;
          CanPrint := True;
          if Assigned(FOnRowPrintQuery) then
            FOnRowPrintQuery(Self, CanPrint);
          if CanPrint then
          begin
            if Assigned(FBeforeRowPrint) then
              FBeforeRowPrint(Self, FRowTop);
            for ix := 0 to FieldCount-1 do
            begin
              if Assigned(FBeforeColPrint) then
                FBeforeColPrint(Self, ix);
              if Fields[ix].Visible then
              begin
                if Fields[ix] is TFloatField then
                  with TFloatField(Fields[ix]) do
                    if DisplayFormat <> '' then //use DisplayFormat if defined
                      OutStr := Format('%*.*s', [ColCharWidths[ix], ColCharWidths[ix],
                        FormatFloat(DisplayFormat, AsFloat)])
                    else if Currency or (Fields[ix] is TCurrencyField) then
                      OutStr := Format('%*m', [ColCharWidths[ix], Fields[ix].AsFloat])
                    else if Precision >= 15 then
                      OutStr := Format('%*.*f', [ColCharWidths[ix], 0, AsFloat])
                    else
                      OutStr := Format('%*.*f', [ColCharWidths[ix], Precision, AsFloat])
                else if Fields[ix] is TNumericField then
//                OutStr := Format('%*.*s', [aWidth, aWidth, Fields[ix].AsString])
                  OutStr := Format('%*.*f', [ColCharWidths[ix], 0, Fields[ix].AsFloat])
                else if Fields[ix] is TDateTimeField then
                  with TDateTimeField(Fields[ix]) do
                    if DisplayFormat <> '' then
                      OutStr := FormatDateTime(DisplayFormat, AsDateTime)
                    else
                      OutStr := AsString
                else  //display as string
                  OutStr := Format('%-*.*s',
                    [ColCharWidths[ix], ColCharWidths[ix], Fields[ix].AsString]);
                TextOut(ColPixLefts[ix], FRowTop, OutStr);
              end;
              if Assigned(FAfterColPrint) then
                FAfterColPrint(Self, ix);
            end;
            if Assigned(FAfterRowPrint) then
              FAfterRowPrint(Self, FRowTop);
            Inc(FRowTop, CharHeight);
          end;
          Next;
        end;
        if Assigned(FAfterFinishPrint) then
          FAfterFinishPrint(Self, FRowTop);
      finally
        GotoBookmark(Bkmk);
        FreeBookmark(Bkmk);
        EnableControls;
      end;
    end;
  finally
    if FAutoClose then Printer.EndDoc;
  end;
end;

procedure TODDataPrinter.SetDataSet(Value: TDataSet);
begin
  FDataSet := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure TODDataPrinter.SetSubTitles(Value: TStrings);
begin
  FSubTitles.Assign(Value);
end;

procedure TODDataPrinter.SetTitleFont(Value: TFont);
begin
  FTitleFont.Assign(Value);
end;

procedure TODDataPrinter.SetSubTitleFont(Value: TFont);
begin
  FSubTitleFont.Assign(Value);
end;

procedure TODDataPrinter.SetHeaderFont(Value: TFont);
begin
  FHeaderFont.Assign(Value);
end;

procedure TODDataPrinter.SetDataFont(Value: TFont);
begin
  FDataFont.Assign(Value);
end;

function TODDataPrinter.GetAbout: string;
begin
  Result := 'Version ' + ODVersion;
  if EvaluationVersion then
    Result := Result + ' demo';
end;

procedure TODDataPrinter.SetAbout(Value: string);
begin
  {do nothing}
end;


end.
