program TextBrain;

uses
  System.StartUpCopy,
  FMX.Forms,
  engine1unit in 'engine1unit.pas',
  engine11unit in 'engine11unit.pas',
  mainformunit in 'mainformunit.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
