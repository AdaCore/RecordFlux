import * as path from 'path';
import { homedir } from 'os';

const folderName = path.basename(__dirname);
export const EXTENSION_ROOT_DIR =
    folderName === 'common' ? path.dirname(path.dirname(__dirname)) : path.dirname(__dirname);
export const BUNDLED_PYTHON_SCRIPTS_DIR = path.join(EXTENSION_ROOT_DIR, '..', 'server');
export const CACHE_DIR = path.join(homedir(), ".cache", "RecordFlux");