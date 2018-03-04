#!/usr/bin/env ruby

# %rbclass%
class %rbclass%
  def initialize
  end

  def start
  end
end

# rubocop:disable IfUnlessModifier
if %rbclass% == $PROGRAM_NAME
  %rbclass%.new.start
end
