::#! -*- perl -*-
@echo off
IF EXIST %0.cmd ( call perl -x %0.cmd %* ) ELSE ( call perl -x %0 %* )
GOTO :eof
::!#
#!/usr/bin/env perl

use strict;
use warnings;
use Cwd;
use Config;
use File::Copy;
use File::Spec::Functions;

use Win32::Job;
use Win32::GUI( qw(IDYES), qw(MB_YESNO), qw(MB_TOPMOST), qw(MB_ICONEXCLAMATION) );

my $main;
my $job = Win32::Job->new;
my ($allo, $cftxt, $dhtxt, $shtxt, $jhtxt, $jotxt, $totxt, $outxt);
my ($DH, $SH, $JH, $JP, $XL, $ML, $CP, @JO);
my ($appc, %apps, %acb, %atf, $optc, %opts, %ocb);
my ($prevopt, $verbose, $rebuild, $doclean, $output) = (-1, 0, 0, 0, "");

initialize();
winall();
exit;

# ----------------------------------------------------------------------

sub initialize {
  if (exists $ENV{DELITE_HOME}) { $DH = "$ENV{DELITE_HOME}"; }
  else { $DH = getcwd(); }
  my ($jhome, $shome) = ("", "");
  open my $bpr, '<', catfile(${DH}, 'build.properties') or die "Can't read old file: $!";
  while ( <$bpr> ) {
    if (/jdk\.home=(\S*)/)   { $jhome = $1; $jhome =~ s/\//\\/g; }
    if (/scala\.home=(\S*)/) { $shome = $1; $shome =~ s/\//\\/g; }
  }
  if (exists $ENV{SCALA_HOME}) { $SH = "$ENV{SCALA_HOME}"; }
  else { $SH = $shome; }
  if (exists $ENV{JAVA_HOME}) { $JH = "$ENV{JAVA_HOME}"; }
  else { $JH = $jhome; }
  $JP = catfile("${JH}", 'bin', 'java');
  $XL = catfile("${DH}", 'scala', 'ext_libs');
  $ML = catfile("${DH}", 'scala', 'src', 'ppl', 'ml', 'apps');
  $CP = join(';',
             catfile(${SH}, 'lib', 'scala-library.jar'),
             catfile(${SH}, 'lib', 'scala-compiler.jar'),
             catfile(${XL}, 'jsr166y.jar'),
             catfile(${XL}, 'ScalaCheck-1.5.jar'),
             catfile(${XL}, 'log4j-1.2.15.jar'),
             catfile(${DH}, 'scala', 'classes'));
  @JO = ("-Xmx1024M", "-cp", "${CP}");

  $appc = catfile("${DH}", 'bin', 'config');
  %apps = loadapps($appc);
  $optc = catfile("${DH}", 'scala', 'src', 'ppl', 'delite', 'config.scala');
  %opts = loadopts($optc);
}

sub saveapps {
  my $dosave = 0;
  for my $k (keys (%atf)) { $dosave = 1 if ($atf{$k}->GetModify()); }
  return unless $dosave;
  my $cfg = shift( @_ );
  open my $ouf, '>', $cfg or die "Can't write config: $!";
  for my $k (sort (keys (%atf))) {
    my $l = $atf{$k}->GetLine(0);
    my $m = $ML;
    $m =~ s/\\/\\\\/g;
    $l =~ s/${m}/\%p/g;
    $l =~ s/\\/\//g;
    print $ouf "$k $l\n";
  }
  close $ouf;
}

sub loadapps {
  my $cfg = shift( @_ );
  my %apps;
  my $num = 1;
  open my $inf, '<', $cfg or die "Can't read old file: $!";
  while ( <$inf> ) {
    my ($key, $name, $args) = split(/\s+/, $_, 3);
    my @arr = ($name);
    foreach my $arg (split(/\s+/, $args)) {
      my @lst = split(/\//, $arg);
      for (@lst) {
        s/\%p/${ML}/;
      }
      @arr = (@arr, catfile(@lst));
    }
    $apps{$key} = [@arr];
  }
  close $inf;
  return %apps;
}

sub loadopts {
  my $cfg = shift( @_ );
  my %opts;
  my $num = 1;
  open my $inf, '<', $cfg or die "Can't read old file: $!";
  while ( <$inf> ) {
    if (/val\s+([^\s=]+)\s*=/) {
        $opts{$1} = $num;
        $num = $num << 1;
    }
  }
  close $inf;
  return %opts;
}

# ----------------------------------------------------------------------

sub gui_Terminate { -1; }

sub allo_Click {
  if ($allo->Checked()) {
    foreach my $opt (keys (%ocb)) {
      $ocb{$opt}->Checked(0);
    }
  }
  return 1;
}

sub verbose_Click {
  if ($verbose) { $verbose = 0; }
  else { $verbose = 1; }
  return 1;
}

sub optset_Click {
  foreach my $opt (keys (%ocb)) {
    $ocb{$opt}->Checked(1);
  }
  $allo->Checked(0);
  return 1;
}

sub optres_Click {
  foreach my $opt (keys (%ocb)) {
    $ocb{$opt}->Checked(0);
  }
  $allo->Checked(0);
  return 1;
}

sub appset_Click {
  foreach my $app (keys (%acb)) {
    $acb{$app}->Checked(1);
  }
  return 1;
}

sub appres_Click {
  foreach my $app (keys (%acb)) {
    $acb{$app}->Checked(0);
  }
  return 1;
}

sub confsv_Click {
  saveapps($appc);
}

sub confld_Click {
  %apps = loadapps($appc);
  foreach my $app (sort (keys (%apps))) {
    $atf{$app}->Text( join(" ", @{ $apps{$app} }));
  }
}

sub expand_filename {
  my $fn = shift( @_ );
  return $fn;
}

sub commit_Click {
  $DH = $dhtxt->GetLine(0) if $dhtxt->GetModify();
  $SH = $shtxt->GetLine(0) if $shtxt->GetModify();
  $JH = $jhtxt->GetLine(0) if $jhtxt->GetModify();
  @JO = split(/\s/, $jotxt->GetLine(0)) if $jotxt->GetModify();
  $appc = $cftxt->GetLine(0) if $cftxt->GetModify();
  $output = expand_filename($outxt->GetLine(0)) if $outxt->GetModify();
  return 1;
}

sub job_handler {
  my $job = shift( @_ );
  my $box = Win32::GUI::MessageBox( $main,
                                    "The tests run too long.  Continue?",
                                    "DELITE Timeout",
                                    MB_YESNO | MB_ICONEXCLAMATION | MB_TOPMOST )
          - IDYES;
  return $box;
}

sub run_Click {
  my ($opinit, $opfini) = (0, (1 << keys (%ocb)) - 1);
  unless ($allo->Checked()) {
    for my $opt (keys (%ocb)) {
      $opinit += $opts{$opt} if $ocb{$opt}->Checked();
    }
    $opfini = $opinit;
  }
  my $exec = "perl";
  my $prog = catfile(${DH}, 'bin', 'runall');
  my @comm = ($exec, $prog, "-delite=$DH", "-scala=$SH", "-java=$JH", "-javaopt=\"@JO\"");
  @comm = (@comm, "-verbose") if $verbose;
  @comm = (@comm, "-doclean") if $doclean;
  for my $thisopt ($opinit .. $opfini) {
    for my $app (sort (keys (%acb))) {
      if ($acb{$app}->Checked()) {
        my $povalue = ($rebuild) ? -1 : $prevopt;
        my @args = (@comm, "-prevopt=$povalue", "-thisopt=$thisopt", "-select=\"$app\"");
        print "@args\n" if $verbose;
        if ($output) {
          $job->spawn($Config{perlpath}, join(' ', @args), {stdout => $output});
        } else {
          $job->spawn($Config{perlpath}, join(' ', @args));
        }
        my $status = $job->watch(\&job_handler, $totxt->GetLine(0), 1);
        $prevopt = $thisopt;
        if ($verbose) {
          my ($s,$m,$h,$d,$o,$y,$w,$a,$i) = localtime();
          print "$h:$m:$s -- ";
          if ($status) { print "test $app succeeded.\n"; }
          else         { print "test $app terminated.\n"; }
        }
      }
    }
  }
  return 1;
}

sub quit_Click {
  print "quit.\n" if $verbose;
  gui_Terminate();
}

sub winall {
  my $bw = 70;
  my $yy = 80;

  $main = Win32::GUI::Window->new( -name => 'gui', -text => 'winall', -resizable => 0,
                                   -left => 600, -top => 4, -width => 600, -height => 700 );
  my $font = Win32::GUI::Font->new( -name => "Lucida Console", -size => 10 );
  my $title = $main->AddLabel(-text => "DELITE TEST CONTROL CENTER", -font => $font);
  $title->Top(10);
  $title->Left(($main->Width() - $title->Width()) / 2);
  $main->AddLabel(-text => "Optimizations", -font => $font, -pos => [50, 50]);
  foreach my $opt (keys (%opts)) {
    $ocb{$opt} = $main->AddCheckbox(-text => "$opt", -pos => [ 40, $yy ],
                                    -onClick => sub {$allo->Checked(0);});
    $yy += 30;
  }
  my $num = 1 << keys(%ocb);
  $allo = $main->AddCheckbox(-name => "allo", -text => "All $num combinations", -pos => [ 40, $yy ]);
  my $reb = $main->AddCheckbox(-name => "rebuild", -text => "Rebuild", -pos => [ 40, $yy + 30 ],
                               -onClick => sub {$rebuild = !$rebuild;} );
  $main->AddCheckbox(-name => "doclean", -text => "Clean Rebuild", -pos => [ 40, $yy + 60 ],
                     -onClick => sub {if ($doclean) { $reb->Enable(); }
                                      else { $rebuild = 1; $reb->Checked(1); $reb->Disable();
                                           } $doclean = !$doclean;});

  $main->AddLabel(-text => "Applications", -font => $font, -pos => [230, 50]);
  $yy = 80;
  foreach my $app (sort (keys (%apps))) {
    $acb{$app} = $main->AddCheckbox(-text => "$app", -pos => [ 200, $yy ]);
    $atf{$app} = $main->AddTextfield( -text   => join(" ", @{ $apps{$app} }),
                                      -left   => 300, -top    => $yy,
                                      -width  => 240, -height =>  20 );
    $yy += 30;
  }

  $main->AddButton(-name => "optset", -align => "center", -text => "Check all",
                   -width => $bw, -pos  => [ 40, $yy ]);
  $main->AddButton(-name => "optres", -align => "center", -text => "Reset all",
                   -width => $bw, -pos  => [ 40, $yy + 30 ]);
  optres_Click();

  $main->AddButton(-name => "appset", -align => "center", -text => "Check all",
                   -width => $bw, -pos  => [ 200, $yy ]);
  $main->AddButton(-name => "appres", -align => "center", -text => "Reset all",
                   -width => $bw, -pos  => [ 200, $yy + 30 ]);
  appset_Click();

  $main->AddButton(-name => "confsv", -align => "center", -text => "Save",
                   -width => $bw, -pos  => [ 300, $yy ]);
  $main->AddButton(-name => "confld", -align => "center", -text => "Load",
                   -width => $bw, -pos  => [ 300, $yy + 30 ]);

  $totxt= $main->AddTextfield( -text   => "120", -left   => 490, -top    => $yy,
                               -width  => 50, -height => 20, -prompt => "Timeout" );
  my $tupc= $main->AddUpDown( -autobuddy => 0, -arrowkeys => 1 );
  $tupc->SetRange(1, 300);
  $tupc->Buddy($totxt);
  $main->AddCheckbox(-name => "verbose", -text => "verbose", -rightbutton => 1,
                     -pos => [ 442, $yy + 30 ]);

  $cftxt = $main->AddTextfield( -prompt => "CONFIG", -text   => "$appc",
                                -left   => 100, -top    => 340,
                                -width  => 400, -height =>  20 );
  $jotxt = $main->AddTextfield( -prompt => "OPTIONS", -text   => join(" ", @JO),
                                -left   => 100, -top    => 380,
                                -width  => 400, -height =>  20 );
  $dhtxt = $main->AddTextfield( -prompt => "DELITE_HOME", -text   => "$DH",
                                -left   => 100, -top    => 420,
                                -width  => 400, -height =>  20 );
  $shtxt = $main->AddTextfield( -prompt => "SCALA_HOME", -text   => "$SH",
                                -left   => 100, -top    => 460,
                                -width  => 400, -height =>  20 );
  $jhtxt = $main->AddTextfield( -prompt => "JAVA_HOME", -text   => "$JH",
                                -left   => 100, -top    => 500,
                                -width  => 400, -height => 20 );
  $outxt = $main->AddTextfield( -prompt => "OUTPUT", -text   => "$output",
                                -left   => 100, -top    => 540,
                                -width  => 400, -height => 20 );
  $main->AddButton( -name  => "commit", -text  => "Commit",
                    -left  => ($main->Width()-$bw)/2, -top   => 620, -width => $bw );

  my $di = ($main->Width() - 3 * $bw) / 4;
  my $b1 = $main->AddButton( -name  => "run", -text => "Run", -width => $bw, -pos => [  $di, 620 ] );
  my $b3 = $main->AddButton( -name  => "quit", -text  => "Quit", -width => $bw,
                             -pos   => [ 3 * $di + 2 * $bw, 620 ] );
  $allo->Checked(1);

  $main->Show();
  Win32::GUI::Dialog();
}

# ----------------------------------------------------------------------
