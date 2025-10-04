import sys
import os

# Configurar el path para las librerías externas
ruta_librerias = os.path.join(os.path.dirname(__file__), '../../mylibrary')
sys.path.append(ruta_librerias)

from RegresionMultiple import *
