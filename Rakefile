task :default => :build
NAME = "merle"
MVERSION = "0.4.9"
CFLAGS = "-fPIC -W -Wall -Werror" 
if File.file?('erlang_config.rb') 
  require 'erlang_config'  
else
  puts "erlang_config.rb file is missing."
  puts "You need to fill it with your local configuration."
  puts "erlang default install is /usr/local."
  File.open("erlang_config.rb",'w') do |file|
    file.write("ERL_TOP=\"/usr/local\"\n")
  end
  exit(-1)
end
 
task :build => ["priv/ketama_erlang_driver"] do
  sh "#{ERL_TOP}/bin/erl -make"
  sh "cp src/#{NAME}.app ebin/#{NAME}.app"
end

directory "priv"
task "priv/ketama_erlang_driver" => ["src/ketama_erlang_driver.c", "priv"] do |t|
  cflags = if uname == "Darwin\n"
     " #{CFLAGS} -DMACOSX "
  else CFLAGS
  end
	sh "gcc #{cflags}  -I. -O3 -lm -lketama -o #{t.name} src/ketama_erlang_driver.c"
end 

desc "installs in $ERL_TOP/lib/"
task :install =>  [:build] do |t|
  destination =  "#{erlang_home}/lib/#{NAME}-#{MVERSION}"
  puts "#{NAME} will be installed in #{destination}"
  sh "mkdir -p #{destination}"
  %w{ priv ebin }.each do |d|
    sh "cp -R #{d} #{destination}/"
  end
end

task :run => [:build] do |t|
  sh "#{ERL_TOP}/bin/erl -pa ebin/ "
end
 
def uname
  @uname ||= `uname`
end 
def erlang_home
    @erlang_home||=IO.popen("#{ERL_TOP}/bin/erl -noinput -noshell -eval 'io:format(code:root_dir()).' -s init stop").readlines[0] 
end