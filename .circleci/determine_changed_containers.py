import os
import re
import json
import logging
import subprocess
from pathlib import Path

GIT_ROOT_DIR = Path(__file__).parent.parent
CIRCLE_CI_FILES_DIR = GIT_ROOT_DIR / 'circle_ci_files'
CIRCLE_CI_FILES_DIR.mkdir(parents=True, exist_ok=True)


def get_changed_files():
    process = subprocess.run(["git", "diff", "--name-only", "HEAD^"], check=True, stdout=subprocess.PIPE)
    return process.stdout.decode('UTF-8').splitlines()


def get_changed_containers(deployment_config):
    changed_files = get_changed_files()
    changed_containers = [name for name, container_description in deployment_config.items() if
                          any(re.match(container_description['is_changed_regexp'], changed_file)
                              for changed_file in changed_files)]
    logging.info(f"list of changed containers: {changed_containers}")
    return changed_containers


def write_dict_as_hash_map(container_config, file_name, key):
    with open(CIRCLE_CI_FILES_DIR / file_name, 'w') as bash_file:
        bash_hash_map = f"""declare -A {file_name}=({" ".join([f'[{name}]="{v[key]}"'
                                                               for name, v in container_config.items()])})\n"""
        bash_file.write(bash_hash_map)


def validate_config(container_config):
    correct_keys = ["image_name", "docker_file_directory", "kubectl_deployment_directories", "is_changed_regexp"]

    image_names = [config['image_name'] for config in container_config.values()]
    if len(image_names) != len(set(image_names)):
        raise ValueError(f"Image names must be unique: {image_names}")

    for name, config in container_config.items():
        for key in correct_keys:
            if key not in config:
                raise ValueError(f"Config for '{name}' misses obligatory key '{key}'")

        kube_config = config['kubectl_deployment_directories']
        if type(kube_config) == dict:
            if not('master' in kube_config and 'not-master' in kube_config):
                raise ValueError(f"If the kubectl_deployment_directories contains a dict you must specify at least "
                                 f"the entries 'master' and 'non-master'. Errored on '{name}'")
        elif type(kube_config) != str:
            raise ValueError(f"The kubectl_deployment_directories for '{name}' has to contain a string or a dict")


def main():
    with open(GIT_ROOT_DIR / '.circleci' / 'deployment_config.json', 'r') as config_file:
        total_config = json.load(config_file)
    logging.info(f"Dump of total_config:\n{json.dumps(total_config, indent=2)}")
    container_config = total_config['containers']
    validate_config(container_config)

    for name in container_config.keys():
        if type(container_config[name]['kubectl_deployment_directories']) == dict:
            if os.environ.get("CIRCLE_BRANCH") == "master":
                container_config[name]['kubectl_deployment_directories'] = \
                    container_config[name]['kubectl_deployment_directories']['master']
            else:
                container_config[name]['kubectl_deployment_directories'] = \
                    container_config[name]['kubectl_deployment_directories']['not-master']

    with open(CIRCLE_CI_FILES_DIR / 'all_containers', 'w') as all_containers_file:
        all_containers_file.writelines([f"{k}\n" for k in container_config.keys()])
    with open(CIRCLE_CI_FILES_DIR / 'changed_containers', 'w') as changed_containers_file:
        changed_containers = get_changed_containers(container_config)
        changed_containers = changed_containers if changed_containers else list(container_config.keys())
        changed_containers_file.writelines([f"{k}\n" for k in changed_containers])

    write_dict_as_hash_map(container_config, 'image_names', "image_name")
    write_dict_as_hash_map(container_config, 'docker_directories', "docker_file_directory")
    write_dict_as_hash_map(container_config, 'kubectl_deployment_directories', 'kubectl_deployment_directories')


if __name__ == '__main__':
    logger = logging.getLogger()
    logger.setLevel(logging.DEBUG)
    main()
