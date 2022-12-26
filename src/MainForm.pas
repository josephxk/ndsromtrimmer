unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, Grids, Objects, ToolWin, Factory,
  StdCtrls, Buttons, ExtCtrls, GlobalConst, GlobalVar, Math, Gauges;

type
  TFormMain = class(TForm)
    MainMenu1: TMainMenu;
    MenuFile: TMenuItem;
    MenuAddROM: TMenuItem;
    StatusBar1: TStatusBar;
    OpenDialogROM: TOpenDialog;
    MenuTools: TMenuItem;
    MenuOptions: TMenuItem;
    CoolBar1: TCoolBar;
    ToolBar1: TToolBar;
    SpeedButtonAdd: TSpeedButton;
    SpeedButtonOptions: TSpeedButton;
    ToolBar2: TToolBar;
    SpeedButtonStart: TSpeedButton;
    BottomPanel: TPanel;
    BitBtnTrim: TBitBtn;
    BitBtnClear: TBitBtn;
    ScrollBox1: TScrollBox;
    HeaderControl1: THeaderControl;
    StringGridROM: TStringGrid;
    ProgressBar1: TProgressBar;
    PopupMenuROMGrid: TPopupMenu;
    Removefromlist1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    VersionLog1: TMenuItem;
    procedure MenuAddROMClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtnTrimClick(Sender: TObject);
    procedure BitBtnClearClick(Sender: TObject);
    procedure MenuOptionsClick(Sender: TObject);
    procedure HeaderControl1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure Removefromlist1Click(Sender: TObject);
    procedure StringGridROMContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure About1Click(Sender: TObject);
    procedure VersionLog1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

  end;

var
  FormMain: TFormMain;

implementation

uses OptionsForm, AboutForm, VersionLogForm;

{$R *.dfm}


procedure TFormMain.FormCreate(Sender: TObject);
begin
  GlobalVar.ROMFileList := TStringList.Create;
  GlobalVar.ObjOptions := TOptionsObject.Create;
  StringGridROM.RowCount := 1;

  HeaderControl1.Sections[0].Text := GlobalConst.ColROMDirHeaderText;
  HeaderControl1.Sections[1].Text := GlobalConst.ColROMFileNameHeaderText;
  HeaderControl1.Sections[2].Text := GlobalConst.ColOrigSizeHeaderText;
  HeaderControl1.Sections[3].Text := GlobalConst.ColEstTrimmedSizeHeaderText;
  HeaderControl1.Sections[4].Text := GlobalConst.ColTrimmedSizeHeaderText;
  HeaderControl1.Sections[5].Text := GlobalConst.ColStatusHeaderText;

  //header control sizing, and prevent resize
  StringGridROM.ColWidths[0] := Trunc((GlobalConst.ColROMDirWeight * StringGridROM.Width) / 100);
  HeaderControl1.Sections[0].Width := Trunc((GlobalConst.ColROMDirWeight * StringGridROM.Width) / 100);
  HeaderControl1.Sections[0].MinWidth := HeaderControl1.Sections[0].Width;
  HeaderControl1.Sections[0].MaxWidth := HeaderControl1.Sections[0].Width;

  StringGridROM.ColWidths[1] := Trunc((GlobalConst.ColROMFileNameWeight * StringGridROM.Width) / 100);
  HeaderControl1.Sections[1].Width := 2 + Trunc((GlobalConst.ColROMFileNameWeight * StringGridROM.Width) / 100);
  HeaderControl1.Sections[1].MinWidth := HeaderControl1.Sections[1].Width;
  HeaderControl1.Sections[1].MaxWidth := HeaderControl1.Sections[1].Width;

  StringGridROM.ColWidths[2] := Trunc((GlobalConst.ColOrigSizeWeight * StringGridROM.Width) / 100);
  HeaderControl1.Sections[2].Width := 1 + Trunc((GlobalConst.ColOrigSizeWeight * StringGridROM.Width) / 100);
  HeaderControl1.Sections[2].MinWidth := HeaderControl1.Sections[2].Width;
  HeaderControl1.Sections[2].MaxWidth := HeaderControl1.Sections[2].Width;

  StringGridROM.ColWidths[3] := Trunc((GlobalConst.ColEstTrimmedSize * StringGridROM.Width) / 100);
  HeaderControl1.Sections[3].Width := 1 + Trunc((GlobalConst.ColEstTrimmedSize * StringGridROM.Width) / 100);
  HeaderControl1.Sections[3].MinWidth := HeaderControl1.Sections[3].Width;
  HeaderControl1.Sections[3].MaxWidth := HeaderControl1.Sections[3].Width;

  StringGridROM.ColWidths[4] := Trunc((GlobalConst.ColTrimmedSizeWeight * StringGridROM.Width) / 100);
  HeaderControl1.Sections[4].Width := 1 + Trunc((GlobalConst.ColTrimmedSizeWeight * StringGridROM.Width) / 100);
  HeaderControl1.Sections[4].MinWidth := HeaderControl1.Sections[4].Width;
  HeaderControl1.Sections[4].MaxWidth := HeaderControl1.Sections[4].Width;

  StringGridROM.ColWidths[5] := -6 + StringGridROM.Width - StringGridROM.ColWidths[0] - StringGridROM.ColWidths[1] - StringGridROM.ColWidths[2] - StringGridROM.ColWidths[3] - StringGridROM.ColWidths[4];
  HeaderControl1.Sections[5].Width := -4 + StringGridROM.Width - StringGridROM.ColWidths[0] - StringGridROM.ColWidths[1] - StringGridROM.ColWidths[2] - StringGridROM.ColWidths[3] - StringGridROM.ColWidths[4];
  HeaderControl1.Sections[5].MinWidth := HeaderControl1.Sections[5].Width;
  HeaderControl1.Sections[5].MaxWidth := HeaderControl1.Sections[5].Width;
end;

procedure TFormMain.MenuAddROMClick(Sender: TObject);
var
  I, ROMSAddCount: Integer;
  LengthOfROMDir, LengthOfROMFileName: Integer;
  ObjROM: TROMObject;
begin
  ROMSAddCount := 0;
  OpenDialogROM.InitialDir := SysUtils.GetCurrentDir;
  if OpenDialogROM.Execute then
  begin
    //visuals
    FormMain.Refresh;
    Screen.Cursor := crHourGlass;
    StatusBar1.Panels.Items[0].Text := 'Adding ROMs...';
    StatusBar1.Refresh;

    //transfer selected ROMs from dialog into ROM array
    RomFileList.BeginUpdate;
    for I := 0 to OpenDialogROM.Files.Count - 1 do
    begin
      //skip this ROM if already exists in array
      if ROMFileList.IndexOf(OpenDialogROM.Files.Strings[I]) <> -1 then
      begin
        continue;
      end;  //end if
      LengthOfROMDir := Length(SysUtils.GetCurrentDir) + 1;
      LengthOfROMFileName := Length(OpenDialogROM.Files.Strings[I]);
      ObjROM := TROMObject.Create;
      ObjROM.FullPath := OpenDialogROM.Files.Strings[I];
      ObjROM.Dir := SysUtils.GetCurrentDir;
      ObjROM.FileName := Copy(OpenDialogROM.Files.Strings[I], LengthOfROMDir + 1, LengthOfROMFileName - LengthOfROMDir);
      ObjROM.OriginalSize := Factory.GetFileSize(ObjROM.FullPath);
      ObjROM.EstTrimmedSize := Factory.GetAppSizeFromROMHeader(ObjROM);
      ObjROM.TrimmedSize := 0;
      ObjROM.Status := GlobalConst.ROMStatusNone;
      ROMFileList.AddObject(OpenDialogROM.Files.Strings[I], ObjROM);
      InsertRowROMGrid(ObjROM, StringGridROM);
      Inc(ROMSAddCount);
    end;  //end for
    ROMFileList.EndUpdate;

    //visuals
    StatusBar1.Panels.Items[0].Text := IntToStr(ROMSAddCount) + ' ROM(s) added';
    StatusBar1.Refresh;
    Screen.Cursor := crDefault;
  end;
end;


procedure TFormMain.BitBtnTrimClick(Sender: TObject);
var
  I: Integer;
  ObjROM: TROMObject;
begin
  if ROMFileList.Count = 0 then
  begin
    MessageDlg('No ROMs to trim!', mtInformation, [mbOK], 0);
    Exit;
  end;
  //visuals
  Screen.Cursor := crHourGlass;
  StatusBar1.Panels.Items[0].Text := 'Please wait...';
  StatusBar1.Refresh;

  //reset ROM status if already trimmed before
  if HasBeenTrimmedBefore then
  begin
    for I := 0 to StringGridROM.RowCount - 1 do
    begin
      ObjROM := TROMObject(StringGridROM.Rows[I].Objects[cnumObject]);
      ResetROMStatusToNotTrimmed(StringGridROM, I);
      UpdateRowROMGrid(ObjROM, StringGridROM, I);
    end;
  end;

  //start trimming ROMs
  for I := 0 to StringGridROM.RowCount - 1 do
  begin
    ObjROM := TROMObject(StringGridROM.Rows[I].Objects[cnumObject]);

    //highlight ROM row
    StringGridROM.Row := I;
    StringGridROM.Refresh;

    //auto select trimming method
    if ObjOptions.TrimMethod = tmAuto then
    begin
      ObjROM.TrimMethod := Factory.ProposeTrimMethod(ObjROM);
      if ObjROM.TrimMethod = tmROMHeaderAppSize then
      begin
        Factory.TrimROMFileUsingROMHeaderAppSize(ObjROM, StringGridROM, I);
      end else if ObjROM.TrimMethod = tmEOF then
      begin
        Factory.TrimROMFileByEOF(ObjROM, StringGridROM, I);
      end;
    //trim using ROM header app size
    end else if ObjOptions.TrimMethod = tmROMHeaderAppSize then
    begin
      Factory.TrimROMFileUsingROMHeaderAppSize(ObjROM, StringGridROM, I);
    //trim by scanning backwards from EOF
    end else if ObjOptions.TrimMethod = tmEOF then
    begin
      Factory.TrimROMFileByEOF(ObjROM, StringGridROM, I);
    end;
    //set global trim variable
    if (ObjROM.TrimmedSize > 0) or (ObjROM.Status = GlobalConst.ROMStatusDone) then
    begin
      HasBeenTrimmedBefore := True;
    end;
  end;  //end for

  //visuals
  Screen.Cursor := crDefault;
  StatusBar1.Panels.Items[0].Text := 'Finished';
  StatusBar1.Refresh;
end;

procedure TFormMain.BitBtnClearClick(Sender: TObject);
var
  I: Integer;
  ObjROM: TROMObject;
  Gauge: TGauge;
begin
  if ROMFileList.Count = 0 then
  begin
    MessageDlg('No ROMs to clear!', mtInformation, [mbOK], 0);
    Exit;
  end;

  //visuals
  Screen.Cursor := crHourGlass;

  for I := 0 to StringGridROM.RowCount - 1 do
  begin
    ObjROM := TROMObject(StringGridROM.Rows[I].Objects[cnumObject]);
    ObjROM.Free;
    Gauge := TGauge(StringGridROM.Rows[I].Objects[cnumStatus]);
    //Gauge.Visible := False;
    Gauge.Free;
    StringGridROM.Rows[I].Clear;
  end;
  StringGridROM.RowCount := 1;
  ROMFileList.Clear;

  //reset global trim variable
  HasBeenTrimmedBefore := (ROMFileList.Count > 0);

  //visuals
  StringGridROM.Refresh;
  StatusBar1.Panels.Items[0].Text := '';
  StatusBar1.Refresh;
  Screen.Cursor := crDefault;
end;

procedure TFormMain.MenuOptionsClick(Sender: TObject);
var
  FormOptions: TFormOptions;
begin
  FormOptions := TFormOptions.Create(Self);
  FormOptions.ShowModal;
  FormOptions.Free;
end;


procedure TFormMain.HeaderControl1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  HeaderControl1.Hint := '';

  //rom name
  if (X > HeaderControl1.Sections[1].Left)
    and (X < HeaderControl1.Sections[1].Right) then
      HeaderControl1.Hint := hcROMFileName;

  //rom file size
  if (X > HeaderControl1.Sections[2].Left)
    and (X < HeaderControl1.Sections[2].Right) then
      HeaderControl1.Hint := hcOrigSize;

  //trimmed info from rom header
  if (X > HeaderControl1.Sections[3].Left)
    and (X < HeaderControl1.Sections[3].Right) then
      HeaderControl1.Hint := hcETS;

  //final size after trimming
  if (X > HeaderControl1.Sections[4].Left)
    and (X < HeaderControl1.Sections[4].Right) then
      HeaderControl1.Hint := hcFTS;

  //status
  if (X > HeaderControl1.Sections[5].Left)
    and (X < HeaderControl1.Sections[5].Right) then
      HeaderControl1.Hint := hcStatus;
end;

procedure TFormMain.Removefromlist1Click(Sender: TObject);
var
  I: Integer;
  ObjROM: TROMObject;
begin
  I := StringGridROM.Row;
  ObjROM := TROMObject(StringGridROM.Rows[I].Objects[cnumObject]);
  if (ObjROM = nil) or SameText(Trim(ObjROM.FullPath), '')then
  begin
    ObjROM.Free;
    MessageDlg('Select a ROM to remove!', mtInformation, [mbOK], 0);
  end else
  begin
    DeleteRowROMGrid(StringGridROM, I);
  end;
end;

procedure TFormMain.StringGridROMContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
var
  I: Integer;
  ObjROM: TROMObject;
begin
  I := StringGridROM.Row;
  ObjROM := TROMObject(StringGridROM.Rows[I].Objects[cnumObject]);
  PopupMenuROMGrid.Items.Find('Remove From List').Enabled := not((ObjROM = nil) or SameText(Trim(ObjROM.FullPath), ''));
end;

procedure TFormMain.About1Click(Sender: TObject);
var
  FormAbout: TFormAbout;
begin
  FormAbout := TFormAbout.Create(nil);
  FormAbout.ShowModal;
  FormAbout.Free;
end;

procedure TFormMain.VersionLog1Click(Sender: TObject);
var
  FormVersionLog: TFormVersionLog;
begin
  FormVersionLog := TFormVersionLog.Create(nil);
  FormVersionLog.ShowModal;
  FormVersionLog.Free;
end;

end.
