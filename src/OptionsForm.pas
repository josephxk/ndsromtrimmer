unit OptionsForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, GlobalConst, Buttons, ComCtrls, ShellAPI, ShlObj,
  Objects, GlobalVar;

type
  TFormOptions = class(TForm)
    BitBtnOK: TBitBtn;
    BitBtnOutputFolder: TBitBtn;
    GroupBoxOF: TGroupBox;
    RBSameAsROMFolder: TRadioButton;
    RBSpecifiedFolder: TRadioButton;
    EditFolder: TEdit;
    RadioGroupTM: TRadioGroup;
    CheckBoxOrigROMFileName: TCheckBox;
    CheckBoxRemoveUnderscores: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure BitBtnOKClick(Sender: TObject);
    procedure BitBtnOutputFolderClick(Sender: TObject);
    procedure RBSpecifiedFolderClick(Sender: TObject);
    procedure RBSameAsROMFolderClick(Sender: TObject);
    
  private
    { Private declarations }
  public
    { Public declarations }
    function BrowseDialog(const Title: String; const Flag: Integer): String;
  end;

var
  FormOptions: TFormOptions;

implementation

{$R *.dfm}

procedure TFormOptions.FormCreate(Sender: TObject);
var
  ne: TNotifyEvent;
begin
  //trim method
  if ObjOptions.TrimMethod = tmAuto then RadioGroupTM.ItemIndex := 0;
  if ObjOptions.TrimMethod = tmROMHeaderAppSize then RadioGroupTM.ItemIndex := 1;
  if ObjOptions.TrimMethod = tmEOF then RadioGroupTM.ItemIndex := 2;

  //output folder
  RBSameAsROMFolder.Checked := ObjOptions.OutputMethod = ofSameFolderAsROM;
  RBSpecifiedFolder.Checked := ObjOptions.OutputMethod = ofSpecifiedFolder;
  BitBtnOutputFolder.Enabled := RBSpecifiedFolder.Checked;
  CheckBoxOrigROMFileName.Enabled := RBSpecifiedFolder.Checked;
  CheckBoxRemoveUnderscores.Enabled := RBSpecifiedFolder.Checked;
  CheckBoxOrigROMFileName.Checked := ObjOptions.KeepOriginalFileName;
  CheckBoxRemoveUnderscores.Checked := ObjOptions.RemoveUnderscores;
  EditFolder.Text := ObjOptions.OutputFolder;
end;

procedure TFormOptions.BitBtnOKClick(Sender: TObject);
begin
  case RadioGroupTM.ItemIndex of
  0:  ObjOptions.TrimMethod := tmAuto;
  1:  ObjOptions.TrimMethod := tmROMHeaderAppSize;
  2:  ObjOptions.TrimMethod := tmEOF;
  end;
  if RBSameAsROMFolder.Checked then ObjOptions.OutputMethod := ofSameFolderAsROM;
  if RBSpecifiedFolder.Checked then ObjOptions.OutputMethod := ofSpecifiedFolder;
  ObjOptions.OutputFolder := EditFolder.Text;
  ObjOptions.KeepOriginalFileName := CheckBoxOrigROMFileName.Checked;
  ObjOptions.RemoveUnderscores := CheckBoxRemoveUnderscores.Checked;
  Self.Close;
end;

function TFormOptions.BrowseDialog(const Title: String; const Flag: Integer): String;
var
  lpItemID : PItemIDList;
  BrowseInfo : TBrowseInfo;
  DisplayName : array[0..MAX_PATH] of char;
  TempPath : array[0..MAX_PATH] of char;
begin
  Result := '';
  FillChar(BrowseInfo, sizeof(TBrowseInfo), #0);
  with BrowseInfo do begin
    //hwndOwner := Application.Handle;
    hwndOwner := Self.Handle;
    pszDisplayName := @DisplayName;
    lpszTitle := PChar(Title);
    ulFlags := Flag;
  end;
  lpItemID := SHBrowseForFolder(BrowseInfo);
  if lpItemId <> nil then begin
    SHGetPathFromIDList(lpItemID, TempPath);
    GlobalFreePtr(lpItemID);
    Result := IncludeTrailingBackslash(TempPath);
  end;
end;

procedure TFormOptions.BitBtnOutputFolderClick(Sender: TObject);
var
  TempDir: String;
begin
  TempDir := BrowseDialog('Select folder:', BIF_RETURNONLYFSDIRS);
  if TempDir <> '' then
  begin
    EditFolder.Text := TempDir;
  end else
  begin
    //
  end;
end;

procedure TFormOptions.RBSpecifiedFolderClick(Sender: TObject);
begin
  CheckBoxOrigROMFileName.Enabled := True;
  CheckBoxRemoveUnderscores.Enabled := True;
  BitBtnOutputFolder.Enabled := True;
end;

procedure TFormOptions.RBSameAsROMFolderClick(Sender: TObject);
begin
  CheckBoxOrigROMFileName.Enabled := False;
  CheckBoxRemoveUnderscores.Enabled := False;
  BitBtnOutputFolder.Enabled := False;
end;

end.
