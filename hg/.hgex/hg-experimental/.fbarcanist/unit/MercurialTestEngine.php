<?php

// TODO: use ArcanistUnitTestEngine once our arcanist supports it
final class MercurialTestEngine extends ArcanistBaseUnitTestEngine {

  public function run() {
    $root = Filesystem::resolvePath($this->getWorkingCopy()->getProjectRoot());
    $json_tmp = new TempFile();

    if ($this->getRunAllTests()) {
      $future = new ExecFuture('%s/scripts/with-hg-dev ./unit.py -j %s --all',
                               $root, $json_tmp);
    } else {
      $future = new ExecFuture('%s/scripts/with-hg-dev ./unit.py -j %s %Ls',
                               $root, $json_tmp, $this->getPaths());
    }
    $future->setCWD(Filesystem::resolvePath($root));
    list($exitcode, $stdout, $stderr) = $future->resolve();
    if ($exitcode == 40) {
      throw new ArcanistUsageException(
        "Please source hg-dev before running unit tests.");
    }
    $result = $this->parseJsonResult($json_tmp);
    return $result;
  }

  protected function supportsRunAllTests() {
    return true;
  }

  protected function parseJsonResult($json_path) {
    $results = [];
    $json = json_decode(Filesystem::readFile($json_path));

    // The JSON looks like the following:
    //   { "test-name.t": {
    //       "result": "success",
    //       "diff": "",
    //       "time": "0.750", } }
    foreach ($json as $name => $info) {
      $result = new ArcanistUnitTestResult();
      $result->setName($name);

      switch ($info->result) {
      case 'failure':
        $result->setResult(ArcanistUnitTestResult::RESULT_FAIL);
        $result->setUserData("```\n".$info->diff."\n```");
        $result->setDuration(floatval($info->time));
        break;
      case 'skip':
        $result->setResult(ArcanistUnitTestResult::RESULT_SKIP);
        break;
      case 'success':
        $result->setResult(ArcanistUnitTestResult::RESULT_PASS);
        $result->setDuration(floatval($info->time));
        break;
      default:
        $result->setResult(ArcanistUnitTestResult::RESULT_UNSOUND);
      }

      $results[] = $result;
    }
    return $results;
  }

}
