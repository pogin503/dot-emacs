#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

# %rbclass%
class %rbclass%
  def initialize
  end

  def start
  end
end

if %file-without-ext% == $PROGRAM_NAME
  %rbclass%.new.start
end
