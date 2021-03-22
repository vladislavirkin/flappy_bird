program flappy_bird_app;

uses
  System.StartUpCopy,
  FMX.Forms,
  main in 'main.pas' {fmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
