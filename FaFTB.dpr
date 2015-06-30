program FaFTB;

uses
  Forms,
  frmMain in 'frmMain.pas' {MainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'FaFTB - Files and Folders Tree Builder 1.20';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
