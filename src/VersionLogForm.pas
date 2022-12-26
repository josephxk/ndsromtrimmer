unit VersionLogForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TFormVersionLog = class(TForm)
    Memo1: TMemo;
    BitBtn1: TBitBtn;
    procedure BitBtn1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormVersionLog: TFormVersionLog;

implementation

{$R *.dfm}

procedure TFormVersionLog.BitBtn1Click(Sender: TObject);
begin
  Self.Close;
end;

end.
