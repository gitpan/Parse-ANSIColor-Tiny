# vim: set ts=2 sts=2 sw=2 expandtab smarttab:
#
# This file is part of Parse-ANSIColor-Tiny
#
# This software is copyright (c) 2011 by Randy Stauner.
#
# This is free software; you can redistribute it and/or modify it under
# the same terms as the Perl 5 programming language system itself.
#
use strict;
use warnings;

package Parse::ANSIColor::Tiny;
{
  $Parse::ANSIColor::Tiny::VERSION = '0.400';
}
BEGIN {
  $Parse::ANSIColor::Tiny::AUTHORITY = 'cpan:RWSTAUNER';
}
# ABSTRACT: Determine attributes of ANSI-Colored string

our @COLORS = qw( black red green yellow blue magenta cyan white );
our %FOREGROUND = (
  (map { (               $COLORS[$_] =>  30 + $_ ) } 0 .. $#COLORS),
  (map { (   'bright_' . $COLORS[$_] =>  90 + $_ ) } 0 .. $#COLORS),
);
our %BACKGROUND = (
  (map { (       'on_' . $COLORS[$_] =>  40 + $_ ) } 0 .. $#COLORS),
  (map { ('on_bright_' . $COLORS[$_] => 100 + $_ ) } 0 .. $#COLORS),
);
our %ATTRIBUTES = (
  clear          => 0,
  reset          => 0,
  bold           => 1,
  dark           => 2,
  faint          => 2,
  underline      => 4,
  underscore     => 4,
  blink          => 5,
  reverse        => 7,
  concealed      => 8,
  reverse_off    => 27,
  %FOREGROUND,
  %BACKGROUND,
);

# copied from Term::ANSIColor
  our %ATTRIBUTES_R;
  # Reverse lookup.  Alphabetically first name for a sequence is preferred.
  for (reverse sort keys %ATTRIBUTES) {
      $ATTRIBUTES_R{$ATTRIBUTES{$_}} = $_;
  }


sub new {
  my $class = shift;
  my $self = {
    @_ == 1 ? %{ $_[0] } : @_,
  };

  $self->{process} = 1
    if $self->{auto_reverse};

  # fix incorrectly specified attributes
  ($self->{background} ||= 'black') =~ s/^(on_)*/on_/;
  ($self->{foreground} ||= 'white') =~ s/^(on_)*//;

  bless $self, $class;
}


sub colors {
  return @COLORS;
}
sub foreground_colors {
  return (@COLORS, map { "bright_$_" } @COLORS);
}
sub background_colors {
  return ( (map { "on_$_" } @COLORS), (map { "on_bright_$_" } @COLORS) );
}


sub identify {
  my ($self, @codes) = @_;
  local $_;
  return
    grep { defined }
    map  { $ATTRIBUTES_R{ 0 + ($_ || 0) } }
    map  { split /;/ }
    # empty sequence is the same as clear so don't let split throw out empty ends
    map  { m/;$/ ? $_ . '0' : $_ }
    # prepending zero doesn't hurt and is probably better than s/^;/0;/
    map  { '0' . ($_ || '0')  }
    @codes;
}


sub normalize {
  my $self = shift;
  my @norm;
  foreach my $attr ( @_ ){
    if( $attr eq 'clear' ){
      @norm = ();
    }
    elsif( $attr eq 'reverse_off' ){
      # reverse_off cancels reverse
      @norm = grep { $_ ne 'reverse' } @norm;
    }
    else {
      # remove previous (duplicate) occurrences of this attribute
      @norm = grep { $_ ne $attr } @norm;
      # new fg color overwrites previous fg
      @norm = grep { !exists $FOREGROUND{$_} } @norm if exists $FOREGROUND{$attr};
      # new bg color overwrites previous bg
      @norm = grep { !exists $BACKGROUND{$_} } @norm if exists $BACKGROUND{$attr};
      push @norm, $attr;
    }
  }
  return @norm;
}


sub parse {
  my ($self, $orig) = @_;

  my $last_pos = 0;
  my $last_attr = [];
  my $processed = [];
  my $parsed = [];

  while( my $matched = $orig =~ m/(\e\[([0-9;]*)m)/mg ){
    my $seq = $1;
    my $attrs = $2;

    # strip out any escape sequences that aren't colors
    # TODO: unicode flags?
    # TODO: make this an option
    #$str =~ s/[^[:print:]]//g;

    my $cur_pos = pos($orig);

    my $len = ($cur_pos - length($seq)) - $last_pos;
    push @$parsed, [
      $processed,
      substr($orig, $last_pos, $len)
    ]
      # don't bother with empty strings
      if $len;

    $last_pos = $cur_pos;
    $last_attr = [$self->normalize(@$last_attr, $self->identify($attrs))];
    $processed = $self->{process} ? [$self->process(@$last_attr)] : $last_attr;
  }

    push @$parsed, [
      $processed,
      substr($orig, $last_pos)
    ]
      # if there's any string left
      if $last_pos < length($orig);

  return $parsed;
}


sub process {
  my ($self, @attr) = @_;
  @attr = $self->process_reverse(@attr) if $self->{auto_reverse};
  return @attr;
}


sub process_reverse {
  my $self = shift;
  my ($rev, $fg, $bg, @attr);
  my $i = 0;
  foreach my $attr ( @_ ){
    if( $attr eq 'reverse' ){
      $rev = 1;
      next;
    }
    elsif( $FOREGROUND{ $attr } ){
      $fg = $i;
    }
    elsif( $BACKGROUND{ $attr } ){
      $bg = $i;
    }
    push @attr, $attr;
    $i++;
  }
  # maintain order for consistency with other methods
  if( $rev ){
    # if either color is missing then the default colors should be reversed
    {
      $attr[ $fg = $i++ ] = $self->{foreground} if !defined $fg;
      $attr[ $bg = $i++ ] = $self->{background} if !defined $bg;
    }
    $attr[ $fg ] = 'on_' . $attr[ $fg ]      if defined $fg;
    $attr[ $bg ] = substr( $attr[ $bg ], 3 ) if defined $bg;
  }
  return @attr;
}


our @EXPORT_OK;
BEGIN {
  my @funcs = qw(identify normalize parse);
  my $suffix = '_ansicolor';
  local $_;
  eval join '', ## no critic (StringyEval)
    map { "sub ${_}$suffix { __PACKAGE__->new->$_(\@_) }" }
    @funcs;
  @EXPORT_OK = map { $_ . $suffix } @funcs;
}

sub import {
  my $class = shift;
  return unless @_;

  my $caller = caller;
  no strict 'refs'; ## no critic (NoStrict)

  foreach my $arg ( @_ ){
    die "'$arg' is not exported by $class"
      unless grep { $arg eq $_ } @EXPORT_OK;
    *{"${caller}::$arg"} = *{"${class}::$arg"}{CODE};
  }
}

# TODO: option for blotting out 'concealed'? s/\S/ /g

1;

# NOTE: this synopsis is tested (eval'ed) in t/synopsis.t


__END__
=pod

=for :stopwords Randy Stauner ACKNOWLEDGEMENTS cpan testmatrix url annocpan anno bugtracker
rt cpants kwalitee diff irc mailto metadata placeholders

=encoding utf-8

=head1 NAME

Parse::ANSIColor::Tiny - Determine attributes of ANSI-Colored string

=head1 VERSION

version 0.400

=head1 SYNOPSIS

  # output from some command
  my $output = "foo\e[31mbar\e[00m";

  my $ansi = Parse::ANSIColor::Tiny->new();
  my $marked = $ansi->parse($output);

  is_deeply
    $marked,
    [
      [ [], 'foo' ],
      [ ['red'], 'bar' ],
    ],
    'parse colored string';

  # don't forget to html-encode the string!
  my $html = join '',
    '<div>',
    (map { '<span class="' . join(' ', @{ $_->[0] }) . '">' . h($_->[1]) . '</span>' } @$marked),
    '</div>';

  is $html,
    '<div><span class="">foo</span><span class="red">bar</span></div>',
    'turned simple ansi into html';

=head1 DESCRIPTION

Parse a string colored with ANSI escape sequences
into a structure suitable for reformatting (into HTML, for example).

The output of terminal commands can be marked up with colors and formatting
that in some instances you'd like to preserve.

This module is essentially the inverse of L<Term::ANSIColor>.
The array refs returned from L</parse>
can be passed back in to C<Term::ANSIColor::colored>.
The strings may not match exactly due to different ways the attributes can be specified,
but the end result should be colored the same.

This is a C<::Tiny> module...
it attempts to be correct for most cases with a small amount of code.
It may not be 100% correct, especially in complex cases.
It only handles the C<m> escape sequence (C<\033[0m>)
which produces colors and simple attributes (bold, underline)
(like what can be produced with L<Term::ANSIColor>).

If you do find bugs please submit tickets (with patches, if possible).

=head1 METHODS

=head2 new

Constructor.

Takes a hash or hash ref of arguments:

=over 4

=item *

C<auto_reverse> - Automatically invert colors when C<reverse> is present; Disabled by default.

=item *

C<background> - Color to assume as background; Black by default. Currently used by L</process_reverse>.

=item *

C<foreground> - Color to assume as foreground; White by default. Currently used by L</process_reverse>.

=back

=head2 colors

Returns a list of the base color names (in numeric escape sequence order).

=head2 foreground_colors

Returns a list of the foreground colors (in numeric escape sequence order).

This includes the base colors and the C<bright_> variants.

=head2 background_colors

Returns a list of the background colors (in numeric escape sequence order).

This includes the C<on_> and C<on_bright_> variants of the base colors.

=head2 identify

  my @names = $parser->identify('1;31');
    # or $parser->identify('1', '31');
  # returns ('bold', 'red')

Identifies attributes by their number;
Returns a B<list> of names.

This is similar to C<uncolor()> in L<Term::ANSIColor>.

Unknown codes will be ignored (remove from the output):

  $parser->identify('33', '52');
  # returns ('yellow') # drops the '52'

=head2 normalize

  my @norm = $parser->normalize(@attributes);

Takes a list of named attributes
(like those returned from L</identify>)
and reduces the list to only those that would have effect.

=over 4

=item *

Duplicates will be removed

=item *

a foreground color will overwrite any previous foreground color (and the previous ones will be removed)

=item *

same for background colors

=item *

C<clear> will remove all previous attributes

=back

  my @norm = $parser->normalize(qw(red bold green));
  # returns ('bold', 'green');

=head2 parse

  my $marked = $parser->parse($output);

Parse the provided string
and return an array ref of array refs describing the formatting:

  # [
  #   [ [], 'plain words' ],
  #   [ ['red'], 'colored words' ],
  # [

These array refs are consistent with the arguments to
C<colored()> in L<Term::ANSIColor>:

  Term::ANSIColor::colored( ['red'], 'colored words' );

=head2 process

Performs post-processing on the provided attributes.

This currently includes L</process_reverse>
if C<auto_reverse> is enabled.

=head2 process_reverse

  my @attr = $parser->process_reverse( $parser->normalize( '31;42;7' ) );

Translates a normalized set of attributes into something easier to process.
This is called internally when C<auto_reverse> is configured.

If C<reverse> is included in the attributes
it should invert the foreground and background colors.

This method makes the attributes more straight forward
and likely easier for other things to process:

  my @norm = $parser->normalize( '1;31;42;7' );
  # returns qw( bold red on_green reverse );

  my @attr = $parser->process_reverse( @norm );
  # returns qw( bold on_red green );

This extra step is necessary to maintain state
and properly handle C<reverse>/C<reverse_off>
since two C<reverse>s do not cancel each other,
but rather the second should be ignored.

If no foreground or background color is currently active
then the colors specified as C<foreground> and C<background>
will be included (and reversed).

  my @attr = $parser->process_reverse( qw( bold reverse ) );
  # returns qw( bold on_white black );

  my @attr = $parser->process_reverse( qw( bold reverse red ) );
  # returns qw( bold on_red   black );

This is consistent with the way it is drawn in the terminal.
Explicitly specifying both colors should make it easy
for anything downstream to process and display as intended.

=head1 FUNCTIONS

=head2 identify_ansicolor

Function wrapped around L</identify>.

=head2 normalize_ansicolor

Function wrapped around L</normalize>.

=head2 parse_ansicolor

Function wrapped around L</parse>.

=head1 EXPORTS

Everything listed in L</FUNCTIONS> is also available for export upon request.

=for test_synopsis sub h { shift };

=head1 SEE ALSO

=over 4

=item *

L<Term::ANSIColor> - For marking up text that will be printed to the terminal

=item *

L<Image::TextMode> (and L<Image::TextMode::Format::ANSI>) - Successor to L<Image::ANSI>; Specifically designed for parsing ANSI art

=item *

L<Term::VT102> - Handles more than colors and is likely more robust but may be overkill in simple situations (and was difficult to install in the past).

=item *

L<HTML::FromANSI::Tiny> - Uses this module to translate ANSI colored text to simple HTML

=back

=head1 SUPPORT

=head2 Perldoc

You can find documentation for this module with the perldoc command.

  perldoc Parse::ANSIColor::Tiny

=head2 Websites

The following websites have more information about this module, and may be of help to you. As always,
in addition to those websites please use your favorite search engine to discover more resources.

=over 4

=item *

Search CPAN

The default CPAN search engine, useful to view POD in HTML format.

L<http://search.cpan.org/dist/Parse-ANSIColor-Tiny>

=item *

RT: CPAN's Bug Tracker

The RT ( Request Tracker ) website is the default bug/issue tracking system for CPAN.

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Parse-ANSIColor-Tiny>

=item *

CPAN Ratings

The CPAN Ratings is a website that allows community ratings and reviews of Perl modules.

L<http://cpanratings.perl.org/d/Parse-ANSIColor-Tiny>

=item *

CPAN Testers

The CPAN Testers is a network of smokers who run automated tests on uploaded CPAN distributions.

L<http://www.cpantesters.org/distro/P/Parse-ANSIColor-Tiny>

=item *

CPAN Testers Matrix

The CPAN Testers Matrix is a website that provides a visual overview of the test results for a distribution on various Perls/platforms.

L<http://matrix.cpantesters.org/?dist=Parse-ANSIColor-Tiny>

=item *

CPAN Testers Dependencies

The CPAN Testers Dependencies is a website that shows a chart of the test results of all dependencies for a distribution.

L<http://deps.cpantesters.org/?module=Parse::ANSIColor::Tiny>

=back

=head2 Bugs / Feature Requests

Please report any bugs or feature requests by email to C<bug-parse-ansicolor-tiny at rt.cpan.org>, or through
the web interface at L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Parse-ANSIColor-Tiny>. You will be automatically notified of any
progress on the request by the system.

=head2 Source Code


L<https://github.com/rwstauner/Parse-ANSIColor-Tiny>

  git clone https://github.com/rwstauner/Parse-ANSIColor-Tiny.git

=head1 AUTHOR

Randy Stauner <rwstauner@cpan.org>

=head1 COPYRIGHT AND LICENSE

This software is copyright (c) 2011 by Randy Stauner.

This is free software; you can redistribute it and/or modify it under
the same terms as the Perl 5 programming language system itself.

=cut

