task :install do
  use_zsh

  install_oh_my_zsh

  install_vundle

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

def install_vundle
  if Dir.exists? "#{Dir.home}/.vim/bundle/vundle"
    puts "Updating vundle"
    `(cd ~/.vim/bundle/vundle && git pull)`
  else
    puts "Installing vundle"
    `git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle`
  end
end

def copy_files
  dotfiles("src").each do |file|
    puts "Copying #{file}"

    if File.dirname(file)
      `mkdir -p ~/#{File.dirname(file)}`
    end
    
    `cp src/#{file} ~/#{file}`
  end
end

# Return all dotfiles in dir and subdirectries, relative to dir and without . and ..
def dotfiles dir
  files = Dir.chdir(dir) do
    Dir.glob("**/*", File::FNM_DOTMATCH) - [".", ".."]
  end

  files.select { |file| File.file? dir + "/" + file }
end
