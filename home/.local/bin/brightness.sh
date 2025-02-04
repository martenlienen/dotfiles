#!/bin/bash

action=$1
brightness=$(brightnessctl -m | grep -Eo '[0-9]+%' | head -c-2)

if [[ "$action" = "increase" ]]; then
  if [[ $brightness -lt 5 ]]; then
    brightnessctl set 1%+
  elif [[ $brightness -lt 10 ]]; then
    brightnessctl set 2%+
  else
    brightnessctl set 5%+
  fi
elif [[ "$action" == "decrease" ]]; then
  if [[ $brightness -lt 5 ]]; then
    brightnessctl set 1%-
  elif [[ $brightness -lt 10 ]]; then
    brightnessctl set 2%-
  else
    brightnessctl set 5%-
  fi
fi
