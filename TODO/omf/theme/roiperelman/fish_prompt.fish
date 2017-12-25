# name: eclm
function _git_branch_name
  echo (command git symbolic-ref HEAD ^/dev/null | sed -e 's|^refs/heads/||')
end

function _is_git_dirty
  echo (command git status -s --ignore-submodules=dirty ^/dev/null)
end

function fish_prompt
  set -l last_status $status
  set -l cyan (set_color -o cyan)
  set -l yellow (set_color -o yellow)
  set -l red (set_color -o red)
  set -l blue (set_color -o blue)
  set -l green (set_color -o green)
  set -l normal (set_color normal)
  set -l white (set_color FFFFFF)
  set -l turquoise (set_color 5fdfff)
  set -l orange (set_color df5f00)
  set -l hotpink (set_color df005f)
  set -l hotpink2 (set_color -o df005f)
  set -l limegreen (set_color 87ff00)
  set -l purple (set_color af5fff)
  set -l purple2 (set_color -o af5fff)
  set -l eta_ebony (set_color 2e3037) # dark
  set -l eta_hopbush (set_color d474ae) # pink
  set -l eta_confetti (set_color d6d14a) # yellow
  set -l eta_apple (set_color 66c856) # green
  set -l eta_pacific_blue (set_color 189ac7) # blue
  set -l eta_turquoise (set_color 32bebe) # teal
  set -l eta_grey_suit (set_color 8b8b8d) # grey

  if test $last_status = 0
      set status_indicator "$green λ "
  else
      set status_indicator "$red λ "
  end

  # set -l cwd $cyan(basename (prompt_pwd))
  set -l cwd $cyan(pwd | sed "s:^$HOME:~:")
  
  if [ (_git_branch_name) ]

    if test (_git_branch_name) = 'master'
      set -l git_branch (_git_branch_name)
      set git_info "$blue git:($hotpink$git_branch$blue)"
    else
      set -l git_branch (_git_branch_name)
      set git_info "$blue git:($hotpink$git_branch$blue)"
    end

    if [ (_is_git_dirty) ]
      set -l dirty "$yellow ✗"
      set git_info "$git_info$dirty"
    end
  end

  # Notify if a command took more than 5 minutes
  if [ "$CMD_DURATION" -gt 300000 ]
    echo The last command took (math "$CMD_DURATION/1000") seconds.
  end

  echo -n -s $white'╭─'$purple2$USER$white ' ' $cwd $git_info $normal\n
  echo -n -s $white'╰─'$status_indicator $normal' '
end
