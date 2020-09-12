
from nltk.corpus import verbnet

classID = verbnet.classids('to kill')

for id in classID:
	print(verbnet.themroles(id))