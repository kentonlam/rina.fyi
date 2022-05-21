import logging
import sys

log = logging.getLogger(__name__)
handler = logging.StreamHandler(sys.stderr)
handler.setFormatter(logging.Formatter('[%(levelname)s:%(module)s:%(funcName)s:%(lineno)d] %(message)s'))
log.addHandler(handler)

log.setLevel(logging.DEBUG)

log.info('asdf')