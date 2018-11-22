import os
import subprocess

id_cmd='xdotool getactivewindow'
id_cmd = subprocess.run(id_cmd.split(' '), stdout=subprocess.PIPE)
id_cmd = str(id_cmd.stdout)
id_cmd = id_cmd[2:-3]
cmd = 'xdotool windowsize --usehints {} 140 50'.format(id_cmd)
#print('>>{}'.format(cmd))
os.system(cmd)
print('>>Run Space Invaders')
os.system('./main')
