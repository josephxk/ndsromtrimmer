unit Factory;

interface

uses
  Types, Grids, Classes, Objects, SysUtils, Dialogs, Math, GlobalConst,
  GlobalVar, Gauges, ComCtrls, Graphics;

  procedure InsertRowROMGrid(ObjROM: TROMObject; var StringGridROM: TStringGrid);
  procedure DeleteRowROMGrid(var StringGridROM: TStringGrid; Row: Integer);
  procedure UpdateRowROMGrid(ObjROM: TROMObject; var StringGridROM: TStringGrid; Row: Integer);
  procedure UpdateROMGrid(ROMFileList: TStrings; var StringGridROM: TStringGrid);
  function GetFileSize(const FileName: String): Integer;
  function TrimROMFileByEOF(var ObjROM: TROMObject; var StringGridROM: TStringGrid; Row: Integer): Boolean;
  function RenameTempROM(const TempFileName: String): Boolean;
  function TrimROMFileUsingROMHeaderAppSize(var ObjROM: TROMObject; var StringGridROM: TStringGrid; Row: Integer): Boolean;
  function GetAppSizeFromROMHeader(var ObjROM: TROMObject): Integer;
  function GenerateOutputFileName(ObjROM: TROMObject): String;
  function ProposeTrimMethod(var ObjROM: TROMObject): Integer;
  function ResetROMStatusToNotTrimmed(var StringGridROM: TStringGrid; Row: Integer): Boolean;
  function GetNextTrimProgressHalfInterval(Current: Integer; Max: Integer): Integer;
  function CreateOutputFolderIfNotExists(FolderName: String; ObjROM: TROMObject): Boolean;

implementation

  procedure UpdateROMGrid(ROMFileList: TStrings; var StringGridROM: TStringGrid);
  var
    I: Integer;
    ObjROM: TROMObject;
  begin
    StringGridROM.RowCount := ROMFileList.Count;
    for I := 0 to ROMFileList.Count - 1 do
    begin
      ObjROM := TROMObject (ROMFileList.Objects[I]);
      StringGridROM.Cells[0, I] := ObjROM.Dir;
      StringGridROM.Cells[1, I] := ObjROM.FileName;
      StringGridROM.Cells[2, I] := IntToStr(Ceil(ObjROM.OriginalSize / 1024));
      if ObjROM.EstTrimmedSize > 0 then
      begin
        StringGridROM.Cells[3, I] := IntToStr(Ceil(ObjROM.EstTrimmedSize / 1024));
      end else if ObjROM.EstTrimmedSize = reBadAppSize then
      begin
        StringGridROM.Cells[3, I] := GlobalConst.TextNA;
      end;
      if ObjROM.TrimmedSize > 0 then
      begin
        StringGridROM.Cells[4, I] := IntToStr(Ceil(ObjROM.TrimmedSize / 1024));
      end else
      begin
        StringGridROM.Cells[4, I] := GlobalConst.TextNA;
      end;
      StringGridROM.Cells[5, I] := ObjROM.Status;
      StringGridROM.Refresh;
    end;  //end for
  end;

  function GetFileSize(const FileName: String): Integer;
  var
    ROMFile: File;
    I: Integer;
  begin
    AssignFile(ROMFile, FileName);
    Reset(ROMFile);
    I := Ceil(FileSize(ROMFile) * 128);
    CloseFile(ROMFile);
    Result := I;
  end;

  function TrimROMFileByEOF(var ObjROM: TROMObject; var StringGridROM: TStringGrid; Row: Integer): Boolean;
  var
    InFs, OutFs: TFileStream;
    OutputFileName: String;
    I, AppSize, NewEOFPos: Integer;
    EOFByte, TempByte: Byte;
    Gauge: TGauge;
  begin
    AppSize := 0;
    NewEOFPos := 0;
    EOFByte := $00;
    TempByte := $00;
    Result := False;
    try
      InFs := TFileStream.Create(ObjROM.FullPath, fmOpenRead OR fmShareDenyNone);
      ObjROM.Status := romStatusAnalyse;  //analysing...
      UpdateRowROMGrid(ObjROM, StringGridROM, Row);
      AppSize := InFs.Size;
      InFs.Seek(AppSize - 1, soFromBeginning);
      InFs.Read(EOFByte, SizeOf(EOFByte));

      //gauge visual
      Gauge := TGauge(StringGridROM.Rows[Row].Objects[cnumStatus]);
      Gauge.MinValue := 0;
      Gauge.MaxValue := AppSize;
      Gauge.Progress := 0;
      Gauge.Visible := True;
      Gauge.Refresh;
      //

      for I := Appsize - 1 downto 0 do
      begin
        //visuals
        Gauge.Progress := AppSize - I;
        
        InFs.Seek(I, soFromBeginning);
        InFs.Read(TempByte, SizeOf(TempByte));
        if EOFByte = TempByte then
        begin
          NewEOFPos := I;
        end else
        begin
          Break;
        end;
      end;

      OutputFileName := GenerateOutputFileName(ObjROM);
      //create output folder if it does not exists
      CreateOutputFolderIfNotExists(ObjOptions.OutputFolder, ObjROM);
      //end create output folder
      OutFs := TFileStream.Create(OutputFileName, fmCreate or fmOpenWrite or fmShareExclusive);
      ObjROM.Status := romStatusTrim;  //trimming...

      //visuals
      Gauge.Progress := GetNextTrimProgressHalfInterval(Gauge.Progress, Gauge.MaxValue);
      UpdateRowROMGrid(ObjROM, StringGridROM, Row);

      InFs.Seek(0, soFromBeginning);
      OutFs.CopyFrom(InFs, NewEOFPos);

      //visuals
      Gauge.Progress := GetNextTrimProgressHalfInterval(Gauge.Progress, Gauge.MaxValue);

      ObjROM.TrimmedSize := OutFs.Size;
      OutFs.Free;
      InFs.Free;
      ObjROM.Status := romStatusDone;//  done
      UpdateRowROMGrid(ObjROM, StringGridROM, Row);

      //visuals
      Gauge.Progress := Gauge.MaxValue;
      Result := True;
    except
      on E: Exception do
      begin

      end;  //end E do
    end;  //end try
  end;

  function TrimROMFileUsingROMHeaderAppSize(var ObjROM: TROMObject; var StringGridROM: TStringGrid; Row: Integer): Boolean;
  var
    InFs, OutFs: TFileStream;
    OutputFileName: String;
    Gauge: TGauge;
  begin
    Result := False;
    //skip this rom if header does not provide valid app size
    if ObjROM.EstTrimmedSize =  reBadAppSize then
    begin
      ObjROM.Status := romStatusSkip;  //skip
      UpdateRowROMGrid(ObjROM, StringGridROM, Row);
      Exit;
    end;
    //gauge visual
    Gauge := TGauge(StringGridROM.Rows[Row].Objects[cnumStatus]);
    Gauge.MinValue := 0;
    Gauge.MaxValue := 100;
    Gauge.Progress := 0;
    Gauge.Visible := True;
    Gauge.Refresh;
    //

    InFs := TFileStream.Create(ObjROM.FullPath, fmOpenRead);// or fmShareDenyNone);
    ObjROM.Status := romStatusAnalyse;  //analysing...

    //visuals
    Gauge.Progress := GetNextTrimProgressHalfInterval(Gauge.Progress, Gauge.MaxValue);
    UpdateRowROMGrid(ObjROM, StringGridROM, Row);

    InFs.Seek(0, soFromBeginning);
    OutputFileName := GenerateOutputFileName(ObjROM);
    //create output folder if it does not exists
    CreateOutputFolderIfNotExists(ObjOptions.OutputFolder, ObjROM);
    //end create output folder
    OutFs := TFileStream.Create(OutputFileName, fmCreate or fmOpenWrite or fmShareExclusive);
    ObjROM.Status := romStatusTrim;  //trimming...

    //visuals
    Gauge.Progress := GetNextTrimProgressHalfInterval(Gauge.Progress, Gauge.MaxValue);
    UpdateRowROMGrid(ObjROM, StringGridROM, Row);

    OutFs.CopyFrom(InFs, ObjROM.EstTrimmedSize);
    ObjROM.TrimmedSize := OutFs.Size;
    OutFs.Free;
    InFs.Free;
    ObjROM.Status := romStatusDone;//  done
    UpdateRowROMGrid(ObjROM, StringGridROM, Row);

    //visuals
    Gauge.Progress := Gauge.MaxValue;
    Result := True;
  end;

  function GetAppSizeFromROMHeader(var ObjROM: TROMObject): Integer;
  var
    InFs: TFileStream;
    TempBtyes: array [1..4] of byte;
  begin
    Result := 0;
    InFs := TFileStream.Create(ObjROM.FullPath, fmOpenRead);
    InFs.Seek($80, soFromBeginning);
    InFs.Read(TempBtyes[1], SizeOf(TempBtyes[1]));
    InFs.Read(TempBtyes[2], SizeOf(TempBtyes[2]));
    InFs.Read(TempBtyes[3], SizeOf(TempBtyes[3]));
    InFs.Read(TempBtyes[4], SizeOf(TempBtyes[4]));
    InFs.Free;
    Result := (TempBtyes[4] shl 24) + (TempBtyes[3] shl 16) + (TempBtyes[2] shl 8) + (TempBtyes[1] shl 0);
    if (Result > ObjROM.OriginalSize) or (Result <= 0) then
    begin
      Result := reBadAppSize;
    end else
    begin
      //
    end;
  end;

  function RenameTempROM(const TempFileName: String): Boolean;
  var
    TempROMFile: File;
  begin
    Result := False;
    Assign(TempROMFile, TempFileName);
    Rename(TempROMFile, Copy(TempFileName, 0, Length(TempFileName) - 8) + GlobalConst.TrimmedROMExt);
    Result := True;
  end;

  function GenerateOutputFileName(ObjROM: TROMObject): String;
  begin
    Result := '';

    //output method
    if ObjOptions.OutputMethod = ofSameFolderAsROM then
    begin
      Result := Copy(ObjROM.FullPath, 0, Length(ObjROM.FullPath) - 4) + GlobalConst.TrimmedROMExt;
    end else
    if ObjOptions.OutputMethod = ofSpecifiedFolder then
    begin
      //keep original file name
      if ObjOptions.KeepOriginalFileName then
      begin
        Result := IncludeTrailingBackslash(ObjOptions.OutputFolder) + ObjROM.FileName;
      end else
      begin
        Result := IncludeTrailingBackslash(ObjOptions.OutputFolder) + Copy(ObjROM.FileName, 0, Length(ObjROM.FileName) - 4) + GlobalConst.TrimmedROMExt;
      end;
      //remove underscores, replace with white space
      if ObjOptions.RemoveUnderscores then
        Result := StringReplace(Result, '_', ' ', [rfReplaceAll]);

    end;
  end;

  function ProposeTrimMethod(var ObjROM: TROMObject): Integer;
  begin
    if ObjROM.EstTrimmedSize = reBadAppSize then
    begin
      Result := tmEOF;
    end else
    begin
      Result := tmROMHeaderAppSize;
    end;
  end;

  function ResetROMStatusToNotTrimmed(var StringGridROM: TStringGrid; Row: Integer): Boolean;
  var
    ObjROM: TROMObject;
    Gauge: TGauge;
  begin
    Result := False;
    ObjROM := TROMObject(StringGridROM.Rows[Row].Objects[cnumObject]);
    ObjROM.TrimmedSize := 0;
    ObjROM.Status := GlobalConst.ROMStatusNone;
    Gauge := TGauge(StringGridROM.Rows[Row].Objects[cnumStatus]);
    Gauge.Progress := 0;
    Gauge.Visible := False;
    Result := True;
  end;

  procedure InsertRowROMGrid(ObjROM: TROMObject; var StringGridROM: TStringGrid);
  var
    I: Integer;
    Rect: TRect;
    Gauge: TGauge;
  begin
    if (StringGridROM.RowCount = 1) and (StringGridROM.Rows[0].Objects[0] = nil) then
    begin
      I := 0;
    end else
    begin
      StringGridROM.RowCount := StringGridROM.RowCount + 1;
      I := StringGridROM.RowCount - 1;
    end;
    StringGridROM.Cells[0, I] := ObjROM.Dir;
    StringGridROM.Cells[1, I] := ObjROM.FileName;
    StringGridROM.Cells[2, I] := IntToStr(Ceil(ObjROM.OriginalSize / 1024));
    if ObjROM.EstTrimmedSize > 0 then
    begin
      StringGridROM.Cells[3, I] := IntToStr(Ceil(ObjROM.EstTrimmedSize / 1024));
    end else if ObjROM.EstTrimmedSize = reBadAppSize then
    begin
      StringGridROM.Cells[3, I] := GlobalConst.TextNA;
    end;
    if ObjROM.TrimmedSize > 0 then
    begin
      StringGridROM.Cells[4, I] := IntToStr(Ceil(ObjROM.TrimmedSize / 1024));
    end else
    begin
      StringGridROM.Cells[4, I] := GlobalConst.TextNA;
    end;
    StringGridROM.Cells[5, I] := ObjROM.Status;
    StringGridROM.Rows[I].Objects[0] := ObjROM;

    //attach a gauge visual object
    Gauge := TGauge.Create(nil);
    Rect := StringGridROM.CellRect(cnumStatus, I);
    Gauge.Top := Gauge.Top + Rect.Top + StringGridROM.GridLineWidth;
    Gauge.Left := Gauge.Left + Rect.Left + StringGridROM.GridLineWidth;
    Gauge.Height := (Rect.Bottom - Rect.Top) - 2;
    Gauge.Width := Rect.Right - Rect.Left - 2;
    Gauge.ForeColor := Graphics.clNavy;
    Gauge.Parent := StringGridROM;
    Gauge.Progress := 0;
    Gauge.Visible := False;
    StringGridROM.Rows[I].Objects[cnumStatus] := Gauge;  //status column
    //

    StringGridROM.Refresh;
  end;

  procedure UpdateRowROMGrid(ObjROM: TROMObject; var StringGridROM: TStringGrid; Row: Integer);
  begin
  {
    StringGridROM.Cells[0, Row] := ObjROM.Dir;
    StringGridROM.Cells[1, Row] := ObjROM.FileName;
    StringGridROM.Cells[2, Row] := IntToStr(Ceil(ObjROM.OriginalSize / 1024));
    if ObjROM.EstTrimmedSize > 0 then
    begin
      StringGridROM.Cells[3, Row] := IntToStr(Ceil(ObjROM.EstTrimmedSize / 1024));
    end else if ObjROM.EstTrimmedSize = reBadAppSize then
    begin
      StringGridROM.Cells[3, Row] := GlobalConst.TextNA;
    end;
    if ObjROM.TrimmedSize > 0 then
    begin
      StringGridROM.Cells[4, Row] := IntToStr(Ceil(ObjROM.TrimmedSize / 1024));
    end else
    begin
      StringGridROM.Cells[4, Row] := GlobalConst.TextNA;
    end;
  }
    StringGridROM.Cells[5, Row] := ObjROM.Status;
    StringGridROM.Refresh;
  end;

  function GetNextTrimProgressHalfInterval(Current: Integer; Max: Integer): Integer;
  begin
    Result := Ceil((Max - Current)/2);
  end;

  function CreateOutputFolderIfNotExists(FolderName: String; ObjROM: TROMObject): Boolean;
  begin
    if ObjOptions.OutputMethod = ofSpecifiedFolder then
    begin
      if not DirectoryExists(ObjOptions.OutputFolder) then
      begin
        if not CreateDir(ObjOptions.OutputFolder) then
        begin
          raise Exception.Create('Cannot create' + ObjOptions.OutputFolder);
        end;
      end;
    end;
  end;

  procedure DeleteRowROMGrid(var StringGridROM: TStringGrid; Row: Integer);
  var
    I: Integer;
    Gauge: TGauge;
    Rect: TRect;
    ObjROM: TROMObject;
    DeleteIndex: Integer;
  begin
    ObjROM := TROMObject(StringGridROM.Rows[Row].Objects[cnumObject]);
    DeleteIndex := ROMFileList.IndexOfObject(ObjROM);
    ROMFileList.Delete(DeleteIndex);
    ObjROM.Free;

    if Row = StringGridROM.RowCount - 1 then
    begin
      Gauge := TGauge(StringGridROM.Rows[Row].Objects[cnumStatus]);
      Gauge.Visible := False;
      Gauge.Free;
      StringGridROM.Rows[Row].Clear;
    end else
    begin
      for I := Row to StringGridROM.RowCount - 2 do
      begin
        //get objects first before delete
        ObjROM := TROMObject(StringGridROM.Rows[I+1].Objects[cnumObject]);
        Gauge := TGauge(StringGridROM.Rows[I+1].Objects[cnumStatus]);
        Rect := StringGridROM.CellRect(cnumStatus, Row);
        Gauge.Top := Rect.Top + StringGridROM.GridLineWidth;
        Gauge.Visible := ObjROM.Status = romStatusDone;
        Gauge.Refresh;
        StringGridROM.Rows[I].Clear;
        StringGridROM.Rows[I].Assign(StringGridROM.Rows[I+1]);
        StringGridROM.Rows[I+1].Clear;
        StringGridROM.Rows[I].Objects[cnumObject] := ObjROM;
        StringGridROM.Rows[I].Objects[cnumStatus] := Gauge;
      end;
    end;
    StringGridROM.RowCount := ROMFileList.Count;
    StringGridROM.Refresh;
  end;
end.
