program endst;

uses
  Forms,
  MainForm in 'MainForm.pas' {FormMain},
  GlobalConst in 'GlobalConst.pas',
  Objects in 'Objects.pas',
  Factory in 'Factory.pas',
  OptionsForm in 'OptionsForm.pas' {FormOptions},
  GlobalVar in 'GlobalVar.pas',
  AboutForm in 'AboutForm.pas' {FormAbout},
  VersionLogForm in 'VersionLogForm.pas' {FormVersionLog};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := '.nds Trimmer';
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
