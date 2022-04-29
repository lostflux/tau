import googletrans

from googletrans import Translator, constants
from pprint import pprint

print(googletrans.LANGUAGES)
translator = Translator()
translator.translate('Hello, world!', dest='swahili')
