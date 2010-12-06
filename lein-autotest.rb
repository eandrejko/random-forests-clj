watch( 'test/.*\.clj' )  {|md| test_stuff }
watch( 'src/.*\.clj' )  {|md| test_stuff }

def colorize(text, color_code)
  "#{color_code}#{text}\e[1;37m"
end

def red(text); colorize(text, "\e[0;31m"); end
def green(text); colorize(text, "\e[0;32m"); end

def match_testing(output)
  for line in output
    if line.match(/Testing.*|0 failures.*|Ran \d tests containing \d assertions.*/)
      print green(line)
    else
      print red(line)
    end
  end
end

def test_stuff
  match_testing(`lein test`)
  puts "======================="
end