task :install do
  use_zsh

  install_oh_my_zsh

  copy_files
end

def use_zsh
  unless ENV["SHELL"] =~ /zsh/
    system %Q{chsh -s `which zsh`}
  end
end

def install_oh_my_zsh
  if Dir.exists? "#{Dir.home}/.oh-my-zsh"
    puts "Updating oh-my-zsh"
    `(cd ~/.oh-my-zsh && git pull)`
  else
    puts "Installing oh-my-zsh"
    `git clone git://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh`
  end
end

def copy_files
  dotfiles("src").each do |file|
    puts "Copying #{file}"

    `cp src/#{file} ~`
  end
end

# Return all dotfiles in dir, relative to dir and without . and ..
def dotfiles dir
  Dir.chdir(dir) do
    Dir.glob("*", File::FNM_DOTMATCH) - [".", ".."]
  end
end
